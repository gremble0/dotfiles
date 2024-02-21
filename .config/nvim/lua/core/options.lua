-- Assigning leader key
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- UI
vim.opt.laststatus = 3
vim.opt.showmode = false
vim.opt.termguicolors = true
vim.opt.cursorline = true
vim.opt.signcolumn = "yes"

-- Indenting
vim.opt.smartindent = true
vim.opt.smarttab = true
vim.opt.tabstop = 4
vim.opt.expandtab = true
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4

-- Mouse and keyboard
vim.opt.clipboard = "unnamedplus"
vim.opt.fillchars = { eob = " " }
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.mouse = "a"
-- TODO listchars instead of ibl

-- Numbers
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.ruler = false

-- Disable nvim intro
vim.opt.shortmess:append "sI"

-- Splitting buffer rules
vim.opt.splitbelow = true
vim.opt.splitright = true

-- Editing
vim.opt.scrolloff = 5
vim.opt.sidescrolloff = 5
vim.opt.undofile = true
vim.opt.timeoutlen = 400
vim.opt.ttimeoutlen = 5
vim.opt.updatetime = 50
vim.opt.pumheight = 8

-- Directories
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 20
