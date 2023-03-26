-- Line numbers
vim.opt.nu = true
vim.opt.relativenumber = true

-- Status bar
vim.opt.laststatus = 2

-- Colors
vim.opt.termguicolors = true

-- Indentation
vim.opt.smartindent = true
vim.opt.smarttab = true
vim.opt.tabstop = 4
vim.opt.expandtab = true
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4

-- Searching
vim.opt.hlsearch = true
vim.opt.incsearch = true

-- Colorcolumn
vim.opt.colorcolumn = "79"

-- Update faster
vim.opt.updatetime = 50

-- Add minimum amount of lines always around cursor
vim.opt.scrolloff = 5
vim.opt.sidescrolloff = 5

-- Dont wrap
vim.opt.wrap = false

-- Disable mouse
vim.opt.mouse = ""

-- Clipboard for yank and paste through xclip
vim.api.nvim_set_option("clipboard", "unnamed")
