-- [[ Default vim options ]]
-- Assigning leader key
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- UI
vim.opt.laststatus = 3
vim.opt.showmode = false
vim.opt.termguicolors = true
vim.opt.cursorline = true
vim.opt.colorcolumn = "79"
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
vim.opt.updatetime = 50 -- Interval for writing swap file to disk, also used by gitsigns

-- Directories
vim.opt.autochdir = true
vim.g.netrw_banner = 0
