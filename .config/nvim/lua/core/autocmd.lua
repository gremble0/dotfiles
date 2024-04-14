local ks = vim.keymap.set
local au = vim.api.nvim_create_autocmd

local file_open_rules_group = vim.api.nvim_create_augroup("FileOpenRules", {})

-- Use 'q' to quickly close these filetypes
au("FileType", {
  group = file_open_rules_group,
  pattern = { "help", "qf", "gitcommit", "fugitive", "oil", "checkhealth" },
  callback = function()
    ks("n", "q", ":close!<CR>", { desc = "Delete current buffer", silent = true, buffer = 0 })
  end,
})

-- Highlight when yanking text
au("TextYankPost", {
  group = vim.api.nvim_create_augroup("HighlightOnYank", {}),
  callback = function()
    vim.highlight.on_yank()
  end,
})
