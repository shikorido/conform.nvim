local dir_manager = require("conform.dir_manager")
local errors = require("conform.errors")
local fs = require("conform.fs")
local ft_to_ext = require("conform.ft_to_ext")
local log = require("conform.log")
local util = require("conform.util")
local uv = vim.uv or vim.loop
local M = {}

---@class (exact) conform.RunOpts
---@field exclusive boolean If true, ensure only a single formatter is running per buffer
---@field dry_run boolean If true, do not apply changes and stop after the first formatter attempts to do so
---@field undojoin boolean Use undojoin to merge formatting changes with previous edit
---@field save_cursorpos boolean Merge cursor line with first diff to make it persistent on undo/redo

---@param formatter_name string
---@param ctx conform.Context
---@param config conform.JobFormatterConfig
---@return string[]
M.build_cmd = function(formatter_name, ctx, config)
  local command = config.command
  if type(command) == "function" then
    command = command(config, ctx)
  end
  local exepath = vim.fn.exepath(command)
  if exepath ~= "" then
    command = exepath
  end
  ---@type string|string[]
  local args = {}
  if ctx.range and config.range_args then
    ---@cast ctx conform.RangeContext
    args = config.range_args(config, ctx)
  elseif config.args then
    local computed_args = config.args
    if type(computed_args) == "function" then
      args = computed_args(config, ctx)
    elseif computed_args then
      args = computed_args
    end
  end

  local function compute_relative_filepath()
    local cwd
    if config.cwd then
      cwd = config.cwd(config, ctx)
    end
    return fs.relative_path(cwd or vim.fn.getcwd(), ctx.filename)
  end

  if type(args) == "string" then
    local interpolated = args
      :gsub("$FILENAME", ctx.filename)
      :gsub("$DIRNAME", ctx.dirname)
      :gsub("$RELATIVE_FILEPATH", compute_relative_filepath)
      :gsub("$EXTENSION", ctx.filename:match(".*(%..*)$") or "")
    return util.shell_build_argv(command .. " " .. interpolated)
  else
    local cmd = { command }
    for _, v in ipairs(args) do
      if v == "$FILENAME" then
        v = ctx.filename
      elseif v == "$DIRNAME" then
        v = ctx.dirname
      elseif v == "$RELATIVE_FILEPATH" then
        v = compute_relative_filepath()
      elseif v == "$EXTENSION" then
        v = ctx.filename:match(".*(%..*)$") or ""
      end
      table.insert(cmd, v)
    end
    return cmd
  end
end

---@param value any
---@return boolean
local function truthy(value)
  return value ~= nil and value ~= false
end

---@param range conform.Range
---@param start_a integer
---@param end_a integer
---@return boolean
local function indices_in_range(range, start_a, end_a)
  return start_a <= range["end"][1] and range["start"][1] <= end_a
end

---@param a? string
---@param b? string
---@return integer
local function common_prefix_len(a, b)
  if not a or not b then
    return 0
  end
  local min_len = math.min(#a, #b)
  for i = 1, min_len do
    if string.byte(a, i) ~= string.byte(b, i) then
      return i - 1
    end
  end
  return min_len
end

---@param a string
---@param b string
---@return integer
local function common_suffix_len(a, b)
  local a_len = #a
  local b_len = #b
  local min_len = math.min(a_len, b_len)
  for i = 0, min_len - 1 do
    if string.byte(a, a_len - i) ~= string.byte(b, b_len - i) then
      return i
    end
  end
  return min_len
end

local function create_text_edit(
  original_lines,
  replacement,
  is_insert,
  is_replace,
  orig_line_start,
  orig_line_end,
  line_ending
)
  local start_line, end_line = orig_line_start - 1, orig_line_end - 1
  local start_char, end_char = 0, 0
  if is_replace then
    -- If we're replacing text, see if we can avoid replacing the entire line
    start_char = common_prefix_len(original_lines[orig_line_start], replacement[1])
    if start_char > 0 then
      replacement[1] = replacement[1]:sub(start_char + 1)
    end

    if original_lines[orig_line_end] then
      local last_line = replacement[#replacement]
      local suffix = common_suffix_len(original_lines[orig_line_end], last_line)
      -- If we're only replacing one line, make sure the prefix/suffix calculations don't overlap
      if orig_line_end == orig_line_start then
        suffix = math.min(suffix, original_lines[orig_line_end]:len() - start_char)
      end
      end_char = original_lines[orig_line_end]:len() - suffix
      if suffix > 0 then
        replacement[#replacement] = last_line:sub(1, last_line:len() - suffix)
      end
    end
  end
  -- If we're inserting text, make sure the text includes a newline at the end.
  -- The one exception is if we're inserting at the end of the file, in which case the newline is
  -- implicit
  if is_insert and start_line < #original_lines then
    table.insert(replacement, "")
  end
  local new_text = table.concat(replacement, line_ending)

  return {
    newText = new_text,
    range = {
      start = {
        line = start_line,
        character = start_char,
      },
      ["end"] = {
        line = end_line,
        character = end_char,
      },
    },
  }
end

---@param bufnr integer
---@param indices integer[][]
---@param original_lines string[]
---@param new_lines string[]
---@return integer[][] new_indices
local function merge_cursor_line_with_first_diff(
  bufnr,
  indices,
  original_lines,
  new_lines
)
  -- NOTE: Delete is an only edge case that breaks cursor pos on redo.
  --       From my tests, insert does not cause the issue.
  -- Lets assume we have a nicely formatted file.
  -- For now, if we insert couple newlines in lua table (or other
  -- syntax constructions that are not aware of newlines)
  -- and then run stylua format with cursor at the end of file.
  -- In result, we get less lines and nvim on redo (after undo)
  -- won't be able to locate cursor line that was deleted,
  -- therefore it places cursor at the beginning of first change.
  --
  -- As possible solutions:
  -- 1. We can append newlines at the end of formatted lines
  --    to preserve line number cursor stays on.
  -- 2. We can set cursor line to the end of formatted lines
  --    before applying format,
  --    initial pos will be lost but no junk left.
  --
  -- I've chosen a second one.

  indices = vim.deepcopy(indices)

  -- Convert inserts and deletes to replaces.
  -- It definetely helps to avoid tweaking issues with delete diff,
  -- I'm not sure about insert, but probably it is also broken.
  -- TODO: Figure out how to avoid doing this.
  for i, idx in ipairs(indices) do
    local orig_line_start, orig_line_count, new_line_start, new_line_count = unpack(idx)

    if orig_line_count == 0 then -- insert to replace
      -- "a\nb\nc\ne"
      -- "a\nb\nc\nd\ne"
      -- (3,0,4,1) - insert
      -- (3,1,3,2) - replace
      --
      -- "a\nb\nc\ne"
      -- "a\nb\nc\nd\ne\nf"
      -- (3,0,4,2) - insert
      -- (3,1,3,3) - replace
      --
      -- "a\nb\nc\nd"
      -- "a\nb\nc\nd\ne\nf"
      -- (4,0,5,2) - insert
      -- (4,1,4,3) - replace
      --
      -- There is an edge cases subset.
      -- "a"
      -- "b\na"
      -- (0,0,1,1) - insert
      -- (0,1,0,2) - replace (wrong, -1..-1 range)
      -- (1,1,1,2) - replace (correct)
      orig_line_count = 1
      new_line_start = new_line_start - 1
      new_line_count = new_line_count + 1
    end

    if new_line_count == 0 then -- delete to replace
      -- "a\nb\nc\nd\ne"
      -- "a\nb\nc\ne"
      -- (4,1,3,0) - delete
      -- (3,2,3,1) - replace
      --
      -- "a\nb\nc\nd\ne\nf"
      -- "a\nb\nc\ne"
      -- (4,2,3,0) - delete
      -- (3,3,3,1) - replace
      --
      -- "a\nb\nc\nd\ne\nf"
      -- "a\nb\nc\nd"
      -- (5,2,4,0) - delete
      -- (4,3,4,1) - replace
      --
      -- There is an edge cases subset.
      -- "a\nb\nc\nd\ne"
      -- "b\nc\n\nd\ne"
      -- (1,1,0,0) - delete
      -- (0,2,0,1) - replace (wrong, -1..0 range and replacement contains nil (tbl[i], i = 0) from util.tbl_slice)
      -- (1,2,1,1) - replace (correct)
      new_line_count = 1
      orig_line_start = orig_line_start - 1
      orig_line_count = orig_line_count + 1
    end

    -- Insert/delete edge cases fix.
    if orig_line_start == 0 then
      orig_line_start = 1
    end
    if new_line_start == 0 then
      new_line_start = 1
    end

    indices[i] = {
      orig_line_start,
      orig_line_count,
      new_line_start,
      new_line_count,
    }
  end

  -- Try to minimize the work if first diff is good.
  local idx = indices[1]
  if not idx then
    return indices
  end

  -- 1,1-indexed, actually we don't need cursor_pos, even if
  -- cursor doesn't intersect with col that was changed,
  -- vim still remembers cursor line and col after change.
  local cursor_line = vim.api.nvim_buf_call(bufnr, function()
    local _, line = unpack(vim.fn.getpos("."))
    return line
  end)

  local orig_line_start, orig_line_count, new_line_start, new_line_count = unpack(idx)

  -- If cursor line is above first diff.
  if orig_line_start > cursor_line and
    new_line_start > cursor_line then
    table.insert(indices, 1, { cursor_line, 1, cursor_line, 1 })
    return indices
  end

  -- Remember first diff start lines.
  local tweaked_orig_first_diff_start = orig_line_start
  local tweaked_new_first_diff_start = new_line_start
  local tweaked_orig_first_diff_count, tweaked_new_first_diff_count

  -- Set maximum clamp counts. Count is treated as exclusive.
  -- For line_start=3 and line_count=1: 3,1 covers only line 3 (3+1-1).
  -- For line_start=3 and line_count=3: 3,3 covers 3,4,5 lines (3+3-1).
  local max_orig_count = #original_lines - orig_line_start + 1
  local max_new_count = #new_lines - new_line_start + 1

  local new_indices = {}

  -- Exclusive end.
  local orig_line_end = orig_line_start + orig_line_count
  local new_line_end = new_line_start + new_line_count

  -- Check if cursor line intersects both orig and new for stable undo/redo.
  -- Avoid insert/delete intentionally (count=0).
  if cursor_line >= orig_line_start and cursor_line < orig_line_end and
    cursor_line >= new_line_start and cursor_line < new_line_end then

    -- Tweak first diff counts.
    tweaked_orig_first_diff_count = orig_line_count
    tweaked_new_first_diff_count = new_line_count

    indices[1] = {
      tweaked_orig_first_diff_start,
      tweaked_orig_first_diff_count,
      tweaked_new_first_diff_start,
      tweaked_new_first_diff_count
    }

    return indices

  else -- comes after both orig and new, or one of
    -- We can't rely on first diff counts to ensure
    -- that orig:new lines mapping is correct at the cursor line.
    -- It will be done in the next for cycle.
    local orig_cursor_lines_count = cursor_line - orig_line_start + 1
    local new_cursor_lines_count = cursor_line - new_line_start + 1
    tweaked_orig_first_diff_count = orig_cursor_lines_count
    tweaked_new_first_diff_count = new_cursor_lines_count
  end

  local any_intersected
  for i = 1, #indices do
    idx = indices[i]

    orig_line_start, orig_line_count, new_line_start, new_line_count = unpack(idx)

    -- The two require special handling I haven't came up with yet.
    --local is_insert = orig_line_count == 0
    --local is_delete = new_line_count == 0

    -- Inclusive end.
    orig_line_end = orig_line_start + orig_line_count - 1
    new_line_end = new_line_start + new_line_count - 1
    local tweaked_orig_first_diff_end = tweaked_orig_first_diff_start + tweaked_orig_first_diff_count - 1
    local tweaked_new_first_diff_end = tweaked_new_first_diff_start + tweaked_new_first_diff_count - 1

    local orig_ends_diff = orig_line_end - tweaked_orig_first_diff_end
    local new_ends_diff = new_line_end - tweaked_new_first_diff_end

    -- Force intersection of the last diff
    -- if there were no previous intersections.
    if i == #indices and not any_intersected then
      any_intersected = true
    end

    -- Since lua has no continue, first if filters out
    -- diffs that are within merged diff.
    if any_intersected or
      tweaked_orig_first_diff_end >= orig_line_end or
      tweaked_new_first_diff_end >= new_line_end then

      -- Check if first diff overlaps with other diffs.
      if any_intersected or orig_ends_diff >= 0 or new_ends_diff >= 0 then
        -- Instead of trying to reduce the diff
        -- lets merge it with our new first diff.
        -- It helps to avoid some complicated logic.

        any_intersected = true

        -- Normalize.
        if orig_ends_diff < 0 or new_ends_diff < 0 then
          local max_neg_abs = math.abs(math.min(orig_ends_diff, new_ends_diff))
          orig_ends_diff = orig_ends_diff + max_neg_abs
          new_ends_diff = new_ends_diff + max_neg_abs
        end

        tweaked_orig_first_diff_count = math.min(tweaked_orig_first_diff_count + orig_ends_diff, max_orig_count)
        tweaked_new_first_diff_count = math.min(tweaked_new_first_diff_count + new_ends_diff, max_new_count)

        -- Is it possible that:

        -- a) new tweaked first diff counts are less than 0? I've not tested this code with insert/delete diffs well.
        if tweaked_orig_first_diff_count < 0 or tweaked_new_first_diff_count < 0 then
          vim.notify(
            "tweaked_orig_first_diff_count or tweaked_new_first_diff_count are less than zero!",
            vim.log.levels.ERROR
          )
        end

        -- Yes, if the line that cursor stayed on was deleted.
        -- b) cursor line can be out-of-scope after the tweaks above?
        -- Inclusive end.
        local tweaked_orig_first_diff_end_new = tweaked_orig_first_diff_start + tweaked_orig_first_diff_count - 1
        local tweaked_new_first_diff_end_new = tweaked_new_first_diff_start + tweaked_new_first_diff_count - 1
        if cursor_line > tweaked_orig_first_diff_end_new or
          cursor_line > tweaked_new_first_diff_end_new then
          --vim.notify(
          --  "cursor_line is out-of-scope of first diff after count tweak:\n"..
          --  "cursor_line="..cursor_line.."\n"..
          --  "#original_lines="..#original_lines.."\n"..
          --  "#new_lines="..#new_lines.."\n"..
          --  "tweaked_orig_first_diff_end_new="..tweaked_orig_first_diff_end_new.."\n"..
          --  "tweaked_new_first_diff_end_new="..tweaked_new_first_diff_end_new,
          --  vim.log.levels.ERROR
          --)
          if cursor_line > #new_lines then
            vim.api.nvim_buf_call(bufnr, function()
              local view = vim.fn.winsaveview()
              local diff = cursor_line - #new_lines
              view.lnum = view.lnum - diff
              view.topline = view.topline - diff
              vim.fn.winrestview(view)
            end)
          end
        end

        -- Shouldn't due to clamping.
        -- c) tweaked diff reached EOF on one or both ends?
        if tweaked_orig_first_diff_end_new > #original_lines or
          tweaked_new_first_diff_end_new > #new_lines then
          vim.notify(
            "one or both ends in a tweaked first diff reached EOF:\n"..
            "tweaked_orig_first_diff_start="..tweaked_orig_first_diff_start.."\n"..
            "tweaked_orig_first_diff_count="..tweaked_orig_first_diff_count.."\n"..
            "tweaked_new_first_diff_start="..tweaked_new_first_diff_start.."\n"..
            "tweaked_new_first_diff_count="..tweaked_new_first_diff_count.."\n"..
            "cursor_line="..cursor_line.."\n"..
            "#original_lines="..#original_lines.."\n"..
            "tweaked_orig_first_diff_end_new="..tweaked_orig_first_diff_end_new.."\n"..
            "#new_lines="..#new_lines.."\n"..
            "tweaked_new_first_diff_end_new="..tweaked_new_first_diff_end_new,
            vim.log.levels.ERROR
          )
        end

        -- Since we've extended our first diff, replace it.
        new_indices[1] = {
          tweaked_orig_first_diff_start,
          tweaked_orig_first_diff_count,
          tweaked_new_first_diff_start,
          tweaked_new_first_diff_count,
        }
      end
    else
      table.insert(new_indices, idx)
    end
  end

  -- Check if new_indices is not empty.
  if #new_indices ~= 0 then
    indices = new_indices
  end

  return indices
end

---@param unified string
---@return integer[][] indices
local function indices_from_unified(unified)
  if #unified == 0 then return {} end

  local lines = vim.split(unified, "\n")
  assert(lines[1]:find("^@@[^@]+@@$"), "Unified diff does not contain @@ .. @@ header")

  local indices = {}
  local diff = "-"
  local no_newline_count = 0

  local orig_line_start, orig_line_count, new_line_start, new_line_count

  for i = 1, #lines do
    local line = lines[i]

    if line:find("^@@[^@]+@@$") or i == #lines then
      if orig_line_start then
        table.insert(indices, {
          orig_line_start,
          orig_line_count,
          new_line_start,
          new_line_count,
        })
      end
    end

    if line:find("^@@[^@]+@@$") then
      orig_line_start, orig_line_count, new_line_start, new_line_count =
        line:gmatch("@@ %-(%d+),?(%d*) %+(%d+),?(%d*) @@")()

      if orig_line_count == "" then orig_line_count = 1 end
      if new_line_count == "" then new_line_count = 1 end

      orig_line_start = tonumber(orig_line_start)
      new_line_start = tonumber(new_line_start)
      orig_line_count = tonumber(orig_line_count)
      new_line_count = tonumber(new_line_count)
    else
      if line:sub(1, 1) == "-" then
        diff = "-"
      elseif line:sub(1, 1) == "+" then
        diff = "+"
      end

      if line:find("\\ No newline at end of file") then
        no_newline_count = no_newline_count + 1

        if diff == "+" then -- new lines lack NL at EOF
          orig_line_count = orig_line_count + 1
        elseif diff == "-" then -- original lines lack NL at EOF
          new_line_count = new_line_count + 1
        end

        -- Undo if original and new lines lack NL at EOF.
        if no_newline_count == 2 then
          orig_line_count = orig_line_count - 1
          new_line_count = new_line_count - 1
        end
      end
    end
  end

  return indices
end

---@param bufnr integer
---@param original_lines string[]
---@param new_lines string[]
---@param range? conform.Range
---@param only_apply_range boolean
---@param dry_run boolean
---@param undojoin boolean
---@param save_cursorpos boolean
---@return boolean any_changes
M.apply_format = function(
  bufnr,
  original_lines,
  new_lines,
  range,
  only_apply_range,
  dry_run,
  undojoin,
  save_cursorpos
)
  if bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end
  if not vim.api.nvim_buf_is_valid(bufnr) then
    return false
  end
  local bufname = vim.api.nvim_buf_get_name(bufnr)
  log.trace("Applying formatting to %s", bufname)
  -- The vim.diff algorithm doesn't handle changes in newline-at-end-of-file well. The unified
  -- result_type has some text to indicate that the eol changed, but the indices result_type has no
  -- such indication. To work around this, we just add a trailing newline to the end of both the old
  -- and the new text.
  --
  -- Remark: The solution won't help in edge cases when there are
  -- 2 newlines at the end and only 1 left after formatting if eol is false (noeol).
  -- Probably because we tend to remove the last NL from formatter
  -- to get vim lines representation if eol is true,
  -- therefore diff gets no NL at eof from new lines.
  -- Lets assume, original lines [19,20] = NL, new lines [19] = NL, [20] - deleted,
  -- i.e. the format config wants 1 stray NL at the end,
  -- which will give (20,1,19,0) indices with an additional syntetic NL ([19,20,21] vs [19,20])
  -- instead of (19,1,18,0).
  -- In (19,1,18,0) case unified diff has no "\ No newline" designation,
  -- it only removes 1 empty line.
  -- Whereas (20,1,19,0) tries to remove non-existent syntetic NL at 21
  -- which was removed from tables of lines.
  -- Syntetic new lines lead to incorrect line numbers occasionally.
  -- I think, only unified diff parsing can be used to
  -- handle such edge cases, otherwise we need to adjust indices result somehow.
  --
  -- Remark 2: Due to the fact how vim handles newlines at eof,
  -- there is no point to have more than 1 nl-at-eof, eol values do not matter,
  -- last NL will always be consumed by vim, even with noeol.
  -- Can be easily checked with `nvim --clean +'set noeol nofixeol' test_file`.
  --table.insert(original_lines, "")
  --table.insert(new_lines, "")
  local original_text = table.concat(original_lines, "\n")
  local new_text = table.concat(new_lines, "\n")
  --table.remove(original_lines)
  --table.remove(new_lines)

  -- Abort if output is empty but input is not (i.e. has some non-whitespace characters).
  -- This is to hack around oddly behaving formatters (e.g black outputs nothing for excluded files).
  if new_text:match("^%s*$") and not original_text:match("^%s*$") then
    log.warn("Aborting because a formatter returned empty output for buffer %s", bufname)
    return false
  end

  log.trace("Comparing lines %s and %s", original_lines, new_lines)
  -----@diagnostic disable-next-line: missing-fields
  --local indices = vim.diff(original_text, new_text, {
  --  result_type = "indices",
  --  algorithm = "histogram",
  --})
  local unified = vim.diff(original_text, new_text, {
    algorithm = "histogram",
  })
  local indices = indices_from_unified(unified)
  assert(type(indices) == "table")
  log.trace("Diff indices %s", indices)

  if save_cursorpos then
    -- Before proceeding to text_edits construction,
    -- we should merge first diff with cursor line
    -- if cursor line is located after first diff,
    -- otherwise simply add 1 line "diff" on cursor line.
    indices = merge_cursor_line_with_first_diff(
      bufnr,
      indices,
      original_lines,
      new_lines
    )
    log.trace("Diff indices after merging cursor line with first diff %s", indices)
    log.trace(
      "Unified diff:\n%s",
      vim.diff(original_text, new_text, {
        algorithm = "histogram"
      })
    )
  end

  local text_edits = {}
  for _, idx in ipairs(indices) do
    local orig_line_start, orig_line_count, new_line_start, new_line_count = unpack(idx)
    local is_insert = orig_line_count == 0
    local is_delete = new_line_count == 0
    local is_replace = not is_insert and not is_delete
    local orig_line_end = orig_line_start + orig_line_count
    local new_line_end = new_line_start + new_line_count
    local replacement = util.tbl_slice(new_lines, new_line_start, new_line_end - 1)

    -- For replacement edits, convert the end line to be inclusive
    if is_replace then
      orig_line_end = orig_line_end - 1
    end

    local should_apply_diff = not only_apply_range
      or not range
      or (is_insert and indices_in_range(range, orig_line_start, orig_line_start + 1))
      or (not is_insert and indices_in_range(range, orig_line_start, orig_line_end))

    -- When the diff is an insert, it actually means to insert after the mentioned line
    if is_insert then
      orig_line_start = orig_line_start + 1
      orig_line_end = orig_line_end + 1
    end

    if should_apply_diff then
      local text_edit = create_text_edit(
        original_lines,
        replacement,
        is_insert,
        is_replace,
        orig_line_start,
        orig_line_end,
        util.buf_line_ending(bufnr)
      )
      table.insert(text_edits, text_edit)

      -- If we're using the aftermarket range formatting, diffs often have paired delete/insert
      -- diffs. We should make sure that if one of them overlaps our selected range, extend the
      -- range so that we pick up the other diff as well.
      if range and only_apply_range then
        range = vim.deepcopy(range)
        range["end"][1] = math.max(range["end"][1], orig_line_end + 1)
      end
    end
  end

  if not dry_run then
    if save_cursorpos then
      log.trace(
        "Original text:\n%s\n"..
        "New text:\n%s\n"..
        "Text edits:\n%s",
        original_text,
        new_text,
        vim.inspect(text_edits)
      )
    end

    local savedview
    if save_cursorpos then
      -- Also save view, because edits application
      -- can shift cursor position. Often happens if diff has less/more lines.
      savedview = vim.api.nvim_buf_call(bufnr, function()
        return vim.fn.winsaveview()
      end)
    end

    log.trace("Applying text edits: %s", text_edits)
    if undojoin then
      -- may fail if after undo
      -- Vim:E790: undojoin is not allowed after undo
      pcall(vim.cmd.undojoin)
    end
    vim.lsp.util.apply_text_edits(text_edits, bufnr, "utf-8")
    log.trace("Done formatting %s", bufname)

    if save_cursorpos then
      vim.api.nvim_buf_call(bufnr, function()
        vim.fn.winrestview(savedview)
      end)
    end
  end

  return not vim.tbl_isempty(text_edits)
end

---@param output? string[]
---@return boolean
local function is_empty_output(output)
  return not output or vim.tbl_isempty(output) or (#output == 1 and output[1] == "")
end

---Map of formatter name to if the last run of that formatter produced an error
---@type table<string, boolean>
local last_run_errored = {}

---@param bufnr integer
---@param formatter conform.FormatterInfo
---@param config conform.FormatterConfig
---@param ctx conform.Context
---@param input_lines string[]
---@param opts conform.RunOpts
---@param callback fun(err?: conform.Error, output?: string[])
---@return integer? job_id
local function run_formatter(bufnr, formatter, config, ctx, input_lines, opts, callback)
  local autocmd_data = {
    formatter = {
      name = formatter.name,
    },
  }
  vim.api.nvim_exec_autocmds("User", {
    pattern = "ConformFormatPre",
    data = autocmd_data,
  })
  log.info("Run %s on %s", formatter.name, vim.api.nvim_buf_get_name(bufnr))
  log.trace("Input lines: %s", input_lines)
  callback = util.wrap_callback(callback, function(err)
    if err then
      if last_run_errored[formatter.name] then
        err.debounce_message = true
      end
      last_run_errored[formatter.name] = true
    else
      last_run_errored[formatter.name] = false
    end
    autocmd_data["err"] = err
    vim.api.nvim_exec_autocmds("User", {
      pattern = "ConformFormatPost",
      data = autocmd_data,
    })
  end)
  if config.format then
    local err_string_cb = function(err, ...)
      if err then
        callback({
          code = errors.ERROR_CODE.RUNTIME,
          message = err,
        }, ...)
      else
        callback(nil, ...)
      end
    end
    ---@cast config conform.LuaFormatterConfig
    local ok, err = pcall(config.format, config, ctx, input_lines, err_string_cb)
    if not ok then
      err_string_cb(string.format("Formatter '%s' error: %s", formatter.name, err))
    end
    return
  end
  ---@cast config conform.JobFormatterConfig
  local cmd = M.build_cmd(formatter.name, ctx, config)
  local cwd = nil
  if config.cwd then
    cwd = config.cwd(config, ctx)
  end
  local env = config.env
  if type(env) == "function" then
    env = env(config, ctx)
  end

  local buffer_text
  -- If the buffer has a newline at the end, make sure we include that in the input to the formatter
  local add_extra_newline = vim.bo[bufnr].eol
  if add_extra_newline then
    table.insert(input_lines, "")
  end
  buffer_text = table.concat(input_lines, "\n")
  if add_extra_newline then
    table.remove(input_lines)
  end

  if not config.stdin then
    log.debug("Creating temp file %s", ctx.filename)
    dir_manager.ensure_parent(ctx.filename)
    local fd = assert(uv.fs_open(ctx.filename, "w", 448)) -- 0700
    uv.fs_write(fd, buffer_text)
    uv.fs_close(fd)
    callback = util.wrap_callback(callback, function()
      log.debug("Cleaning up temp file %s", ctx.filename)
      uv.fs_unlink(ctx.filename)
      dir_manager.cleanup()
    end)
  end

  log.debug("Run command: %s", cmd)
  if cwd then
    log.debug("Run CWD: %s", cwd)
  else
    log.debug("Run default CWD: %s", vim.fn.getcwd())
  end
  if env then
    log.debug("Run ENV: %s", env)
  end
  local exit_codes = config.exit_codes or { 0 }
  local pid
  local ok, job_or_err = pcall(
    vim.system,
    cmd,
    {
      cwd = cwd,
      env = env,
      stdin = config.stdin and buffer_text or nil,
      text = true,
    },
    vim.schedule_wrap(function(result)
      local code = result.code
      local stdout = result.stdout and vim.split(result.stdout, "\r?\n") or {}
      local stderr = result.stderr and vim.split(result.stderr, "\r?\n") or {}
      if vim.tbl_contains(exit_codes, code) then
        local output = stdout
        if not config.stdin then
          local fd = assert(uv.fs_open(ctx.filename, "r", 448)) -- 0700
          local stat = assert(uv.fs_fstat(fd))
          local content = assert(uv.fs_read(fd, stat.size))
          uv.fs_close(fd)
          output = vim.split(content, "\r?\n")
        end
        -- Remove the trailing newline from the output to convert back to vim lines representation
        if add_extra_newline and output[#output] == "" then
          table.remove(output)
        end
        -- Vim will never let the lines array be empty. An empty file will still look like { "" }
        if #output == 0 then
          table.insert(output, "")
        end
        log.debug("%s exited with code %d", formatter.name, code)
        log.trace("Output lines: %s", output)
        log.trace("%s stderr: %s", formatter.name, stderr)
        callback(nil, output)
      else
        log.info("%s exited with code %d", formatter.name, code)
        log.debug("%s stdout: %s", formatter.name, stdout)
        log.debug("%s stderr: %s", formatter.name, stderr)
        local err_str
        if not is_empty_output(stderr) then
          err_str = table.concat(stderr, "\n")
        elseif not is_empty_output(stdout) then
          err_str = table.concat(stdout, "\n")
        else
          err_str = "unknown error"
        end
        if
          vim.api.nvim_buf_is_valid(bufnr)
          and pid ~= vim.b[bufnr].conform_pid
          and opts.exclusive
        then
          callback({
            code = errors.ERROR_CODE.INTERRUPTED,
            message = string.format("Formatter '%s' was interrupted", formatter.name),
          })
        else
          callback({
            code = errors.ERROR_CODE.RUNTIME,
            message = string.format("Formatter '%s' error: %s", formatter.name, err_str),
          })
        end
      end
    end)
  )
  if not ok then
    callback({
      code = errors.ERROR_CODE.VIM_SYSTEM,
      message = string.format("Formatter '%s' error in vim.system: %s", formatter.name, job_or_err),
    })
    return
  end
  pid = job_or_err.pid
  if opts.exclusive then
    vim.b[bufnr].conform_pid = pid
  end

  return pid
end

---@param bufnr integer
---@param config conform.FormatterConfig
---@param range? conform.Range
---@return conform.Context
M.build_context = function(bufnr, config, range)
  if bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end
  local filename = vim.api.nvim_buf_get_name(bufnr)

  local shiftwidth = vim.bo[bufnr].shiftwidth
  if shiftwidth == 0 then
    shiftwidth = vim.bo[bufnr].tabstop
  end

  -- Hack around checkhealth. For buffers that are not files, we need to fabricate a filename
  if vim.bo[bufnr].buftype ~= "" then
    filename = ""
  end
  local dirname
  if filename == "" then
    dirname = vim.fn.getcwd()
    filename = fs.join(dirname, "unnamed_temp")
    local ft = vim.bo[bufnr].filetype
    if ft and ft ~= "" then
      filename = filename .. "." .. (ft_to_ext[ft] or ft)
    end
  else
    dirname = vim.fs.dirname(filename)
  end

  if not config.stdin then
    local template = config.tmpfile_format
    if not template then
      template = ".conform.$RANDOM.$FILENAME"
    end
    local basename = vim.fs.basename(filename)
    local tmpname =
      template:gsub("$RANDOM", tostring(math.random(1000000, 9999999))):gsub("$FILENAME", basename)
    if fs.is_absolute(tmpname) then
      filename = tmpname
    else
      local parent = vim.fs.dirname(filename)
      filename = fs.join(parent, tmpname)
    end
  end
  return {
    buf = bufnr,
    filename = filename,
    dirname = dirname,
    range = range,
    shiftwidth = shiftwidth,
  }
end

---@param bufnr integer
---@param formatters conform.FormatterInfo[]
---@param range? conform.Range
---@param opts conform.RunOpts
---@param callback fun(err?: conform.Error, did_edit?: boolean)
M.format_async = function(bufnr, formatters, range, opts, callback)
  if bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end

  -- kill previous jobs for buffer
  local prev_pid = vim.b[bufnr].conform_pid
  if prev_pid and opts.exclusive then
    if uv.kill(prev_pid) == 0 then
      log.info("Canceled previous format job for %s", vim.api.nvim_buf_get_name(bufnr))
    end
  end

  local original_lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  local changedtick = vim.b[bufnr].changedtick
  M.format_lines_async(
    bufnr,
    formatters,
    range,
    original_lines,
    opts,
    function(err, output_lines, all_support_range_formatting)
      local did_edit = nil
      -- discard formatting if buffer has changed
      if not vim.api.nvim_buf_is_valid(bufnr) or changedtick ~= util.buf_get_changedtick(bufnr) then
        err = {
          code = errors.ERROR_CODE.CONCURRENT_MODIFICATION,
          message = string.format(
            "Async formatter discarding changes for %d: concurrent modification",
            bufnr
          ),
        }
      else
        did_edit = M.apply_format(
          bufnr,
          original_lines,
          output_lines,
          range,
          not all_support_range_formatting,
          opts.dry_run,
          opts.undojoin,
          opts.save_cursorpos
        )
      end
      callback(err, did_edit)
    end
  )
end

---@param bufnr integer
---@param formatters conform.FormatterInfo[]
---@param range? conform.Range
---@param input_lines string[]
---@param opts conform.RunOpts
---@param callback fun(err?: conform.Error, output_lines: string[], all_support_range_formatting: boolean)
M.format_lines_async = function(bufnr, formatters, range, input_lines, opts, callback)
  if bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end
  local idx = 1
  local all_support_range_formatting = true
  local final_err = nil

  local function run_next_formatter()
    local formatter = formatters[idx]
    if not formatter then
      callback(final_err, input_lines, all_support_range_formatting)
      return
    end
    idx = idx + 1

    local config = assert(require("conform").get_formatter_config(formatter.name, bufnr))
    local ctx = M.build_context(bufnr, config, range)
    run_formatter(bufnr, formatter, config, ctx, input_lines, opts, function(err, output)
      if err then
        final_err = errors.coalesce(final_err, err)
      end
      input_lines = output or input_lines
      all_support_range_formatting = all_support_range_formatting and truthy(config.range_args)
      run_next_formatter()
    end)
  end
  run_next_formatter()
end

---@param bufnr integer
---@param formatters conform.FormatterInfo[]
---@param timeout_ms integer
---@param range? conform.Range
---@param opts conform.RunOpts
---@return conform.Error? error
---@return boolean did_edit
M.format_sync = function(bufnr, formatters, timeout_ms, range, opts)
  if bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end
  local original_lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

  -- kill previous jobs for buffer
  local prev_pid = vim.b[bufnr].conform_pid
  if prev_pid and opts.exclusive then
    if uv.kill(prev_pid) == 0 then
      log.info("Canceled previous format job for %s", vim.api.nvim_buf_get_name(bufnr))
    end
  end

  local err, final_result, all_support_range_formatting =
    M.format_lines_sync(bufnr, formatters, timeout_ms, range, original_lines, opts)

  local did_edit = M.apply_format(
    bufnr,
    original_lines,
    final_result,
    range,
    not all_support_range_formatting,
    opts.dry_run,
    opts.undojoin,
    opts.save_cursorpos
  )
  return err, did_edit
end

---@param bufnr integer
---@param formatters conform.FormatterInfo[]
---@param timeout_ms integer
---@param range? conform.Range
---@param opts conform.RunOpts
---@return conform.Error? error
---@return string[] output_lines
---@return boolean all_support_range_formatting
M.format_lines_sync = function(bufnr, formatters, timeout_ms, range, input_lines, opts)
  if bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end
  local start = uv.hrtime() / 1e6

  local all_support_range_formatting = true
  local final_err = nil
  for _, formatter in ipairs(formatters) do
    local remaining = timeout_ms - (uv.hrtime() / 1e6 - start)
    if remaining <= 0 then
      return errors.coalesce(final_err, {
        code = errors.ERROR_CODE.TIMEOUT,
        message = string.format("Formatter '%s' timeout", formatter.name),
      }),
        input_lines,
        all_support_range_formatting
    end
    local done = false
    local result = nil
    ---@type conform.FormatterConfig
    local config = assert(require("conform").get_formatter_config(formatter.name, bufnr))
    local ctx = M.build_context(bufnr, config, range)
    local pid = run_formatter(
      bufnr,
      formatter,
      config,
      ctx,
      input_lines,
      opts,
      function(err, output)
        final_err = errors.coalesce(final_err, err)
        done = true
        result = output
      end
    )
    all_support_range_formatting = all_support_range_formatting and truthy(config.range_args)

    local wait_result, wait_reason = vim.wait(remaining, function()
      return done
    end, 5)

    if not wait_result then
      if pid then
        uv.kill(pid)
      end
      if wait_reason == -1 then
        return errors.coalesce(final_err, {
          code = errors.ERROR_CODE.TIMEOUT,
          message = string.format("Formatter '%s' timeout", formatter.name),
        }),
          input_lines,
          all_support_range_formatting
      else
        return errors.coalesce(final_err, {
          code = errors.ERROR_CODE.INTERRUPTED,
          message = string.format("Formatter '%s' was interrupted", formatter.name),
        }),
          input_lines,
          all_support_range_formatting
      end
    end

    input_lines = result or input_lines
  end

  return final_err, input_lines, all_support_range_formatting
end

return M
