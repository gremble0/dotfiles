-- [[ Keymaps ]]
--- NORMAL MODE KEYBINDS
-- Faster navigation between windows
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "Move to window to the left" })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Move to window below" })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Move to window above" })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "Move to window to the right" })

-- Buffer navigation
vim.keymap.set("n", "<C-c>", "<C-w>c", { desc = "Close buffer", silent = true })

-- Move current line up or down
vim.keymap.set("n", "<leader>j", ":m .+1<CR>", { desc = "Move current line down", silent = true })
vim.keymap.set("n", "<leader>k", ":m .-2<CR>", { desc = "Move current line up", silent = true })

-- Clear highlights with escape
vim.keymap.set("n", "<Esc>", ":noh<CR>", { desc = "Clear highlights" })

-- Remap for dealing with word wrap
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'",
    { desc = "Move up with word wrapping", expr = true, silent = true })
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'",
    { desc = "Move down with word wrapping", expr = true, silent = true })

-- Open netrw
vim.keymap.set("n", "<C-e>", ":Ex<CR>", { desc = "Open netrw" })

-- Diagnostic keymaps
vim.keymap.set("n", "gp", vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })
vim.keymap.set("n", "gn", vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "Open floating diagnostic message" })

--- VISUAL MODE KEYBINDS
vim.keymap.set("v", "<leader>j", "dp", { desc = "Move selected lines down" })
vim.keymap.set("v", "<leader>k", "dkP", { desc = "Move selected lines up" })

--- INSERT MODE KEYBINDS
vim.keymap.set("i", "<C-h>", "<Left>", { desc = "Move left" })
vim.keymap.set("i", "<C-j>", "<Down>", { desc = "Move down" })
vim.keymap.set("i", "<C-k>", "<Up>", { desc = "Move up" })
vim.keymap.set("i", "<C-l>", "<Right>", { desc = "Move right" })
