---@type conform.FileLuaFormatterConfig
return {
  meta = {
    url = "https://github.com/shikorido/conform.nvim/blob/save-cursorpos/lua/conform/formatters/vim_indent.lua",
    description = "Use builtin vim indent for the given filetype (usually indentexpr set by ftplugin).",
  },
  -- self == config, lines == input_lines
  format = function(self, ctx, input_lines, callback)
    local bufnr = ctx.buf

    -- Create a temporary scratch buffer.
    local temp_buf = vim.api.nvim_create_buf(false, true)

    -- Copy buffer options to the temp buffer.
    local opts = {
      "ts", "sts", "sw", "et", "ft", "ff"
      -- The following options can be omitted.
      --"smarttab" -- window opt, does not work for buf.
      --"smartindent", "autoindent",
      --"cindent", "indentexpr"
    }
    for _, opt in ipairs(opts) do
      local sval = vim.api.nvim_get_option_value(opt, { buf = bufnr })
      vim.api.nvim_set_option_value(opt, sval, { buf = temp_buf })
    end

    -- Copy lines from the original buffer to the temporary buffer.
    vim.api.nvim_buf_set_lines(temp_buf, 0, -1, false, input_lines)

    if not ctx.range then
      -- Run gg=G in the temporary buffer to indent everything.
      vim.api.nvim_buf_call(temp_buf, function() vim.cmd("silent normal! gg=G") end)
    else
      -- Indent range only.
      local start_line = unpack(ctx.range.start)
      local end_line = unpack(ctx.range["end"])
      -- The following %dG=%dG action (and gg=G for a full buffer)
      -- does not reset mode, unlike
      -- "<", ">", "gv="
      -- and
      -- ".", mode, ".", "="
      vim.api.nvim_buf_call(temp_buf, function()
        vim.cmd(
          string.format(
            "silent normal! %dG=%dG",
            start_line,
            end_line
          )
        )
      end)
    end

    -- Get modified lines from the temporary buffer.
    local out_lines = vim.api.nvim_buf_get_lines(temp_buf, 0, -1, false)

    -- Clean up the temporary buffer.
    vim.api.nvim_buf_delete(temp_buf, { force = true })

    callback(nil, out_lines)
  end,
}
