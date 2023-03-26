vim.g.mapleader = " "

-- Open netrw
vim.keymap.set("n", "<leader>ex", vim.cmd.Ex)

-- Open terminal
vim.keymap.set("n", "<F1>", ":term<enter><C-w>L")

-- Navigate faster while holding shift
vim.keymap.set("n", "<S-h>", "5h")
vim.keymap.set("n", "<S-j>", "5j")
vim.keymap.set("n", "<S-k>", "5k")
vim.keymap.set("n", "<S-l>", "5l")
vim.keymap.set("n", "E", "dd")

-- Replace all occurances in file
vim.keymap.set("n", "%", ":%s/")

-- Delete line in single keystroke
vim.keymap.set("n", "E", "dd")

-- Vertical movement
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
