local com = require("core.common")
local ks = vim.keymap.set

--- NORMAL MODE KEYBINDS
-- Faster navigation between windows
ks("n", "<C-h>", "<C-w>h", { desc = "Move to window to the left" })
ks("n", "<C-j>", "<C-w>j", { desc = "Move to window below" })
ks("n", "<C-k>", "<C-w>k", { desc = "Move to window above" })
ks("n", "<C-l>", "<C-w>l", { desc = "Move to window to the right" })

ks("n", "<C-c>", com.close_win_or_buffer, { desc = "Close buffer", silent = true })

-- Clear highlights with escape
ks("n", "<Esc>", ":noh<CR>", { desc = "Clear highlights" })

-- Remap for dealing with word wrap
ks("n", "k", "v:count == 0 ? 'gk' : 'k'", { desc = "Move up with word wrapping", expr = true, silent = true })
ks("n", "j", "v:count == 0 ? 'gj' : 'j'", { desc = "Move down with word wrapping", expr = true, silent = true })

-- Diagnostic keymaps
ks("n", "gp", vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })
ks("n", "gn", vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })
ks("n", "<leader>e", vim.diagnostic.open_float, { desc = "Open floating diagnostic message" })

--- VISUAL MODE KEYBINDS
ks("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selected lines down", silent = true })
ks("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selected lines up", silent = true })

-- Better indentation
ks("v", ">", ">gv", { desc = "Indent and reselect", silent = true })
ks("v", "<", "<gv", { desc = "Dedent and reselect", silent = true })

--- INSERT MODE KEYBINDS
ks("i", "<C-h>", "<Left>", { desc = "Move left" })
ks("i", "<C-j>", "<Down>", { desc = "Move down" })
ks("i", "<C-k>", "<Up>", { desc = "Move up" })
ks("i", "<C-l>", "<Right>", { desc = "Move right" })
ks("i", "<C-a>", "<Home>", { desc = "Move to the start of the line" })
ks("i", "<C-e>", "<End>", { desc = "Move to the end of the line" })
