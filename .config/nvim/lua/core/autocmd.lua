local com = require("core.common")
local ks = vim.keymap.set

local file_open_rules_group = vim.api.nvim_create_augroup("FileOpenRules", {})

-- Use 'q' to quickly close these filetypes
vim.api.nvim_create_autocmd("FileType", {
  group = file_open_rules_group,
  pattern = { "help", "qf", "gitcommit", "fugitive", "oil", "checkhealth" },
  callback = function()
    ks("n", "q", com.close_win_or_buffer, { desc = "Delete current buffer", buffer = 0 })
  end,
})

-- Enter insert mode in gitcommit buffers
vim.api.nvim_create_autocmd("FileType", {
  group = file_open_rules_group,
  pattern = "gitcommit",
  callback = function()
    vim.cmd("startinsert")
  end,
})

-- Highlight when yanking text
vim.api.nvim_create_autocmd("TextYankPost", {
  group = vim.api.nvim_create_augroup("HighlightOnYank", {}),
  callback = function()
    vim.highlight.on_yank()
  end,
})
