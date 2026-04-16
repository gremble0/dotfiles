local ks = vim.keymap.set
local au = vim.api.nvim_create_autocmd

local file_open_rules_group = vim.api.nvim_create_augroup("FileOpenRules", {})

-- Use 'q' to quickly close these filetypes
au("FileType", {
  group = file_open_rules_group,
  pattern = { "help", "qf", "git", "gitcommit", "fugitive", "fugitiveblame", "oil", "checkhealth" },
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

au({ "BufEnter", "InsertLeave" }, {
  callback = function()
    vim.fn.matchadd("TrailingWhitespace", "\\s\\+$")
  end,
})
vim.api.nvim_set_hl(0, "TrailingWhitespace", { bg = "#FF0000" })
