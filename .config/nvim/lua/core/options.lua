-- Assigning leader key
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- UI
vim.opt.showmode = false
vim.opt.termguicolors = true
vim.opt.cursorline = true
vim.opt.signcolumn = "yes"

-- Menus
vim.opt.completeopt = { "menu", "menuone" }

-- Search
vim.opt.inccommand = "split"

-- Indenting
vim.opt.smartindent = true
vim.opt.breakindent = true
vim.opt.smarttab = true
vim.opt.expandtab = true
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2

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
vim.opt.shortmess:append("sI")

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
vim.opt.swapfile = false
