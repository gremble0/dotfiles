vim.api.nvim_create_user_command("W", function(opts)
  vim.cmd("w" .. opts.bang and "!" or "")
end, { bang = true })

vim.api.nvim_create_user_command("E", function(opts)
  vim.cmd("e" .. opts.bang and "!" or "")
end, { bang = true })
