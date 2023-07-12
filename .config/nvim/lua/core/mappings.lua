-- [[ Keymaps ]]
--- NORMAL MODE KEYBINDS ---
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

-- Use leader key instead of hitting same key twice for some commands
vim.keymap.set("n", "<leader>d", "dd", { desc = "delete line" })
vim.keymap.set("n", "<leader>y", "yy", { desc = "yank line" })
vim.keymap.set("n", "<leader>z", "zz", { desc = "center camera" })

-- Switch between windows
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "move to window to the left" })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "move to window below" })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "move to window above" })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "move to window to the right" })

-- Switch to previous buffer
vim.keymap.set("n", "<C-p>", ":b#<CR>:echo \"Switched to previous buffer\"<CR>", { desc = "move to window to the right" })

-- Move current line up or down 
vim.keymap.set("n", "<leader>j", ":m .+1<CR>", { desc = "move current line down" })
vim.keymap.set("n", "<leader>k", ":m .-2<CR>", { desc = "move current line up" })

-- Clear highlights with escape
vim.keymap.set("n", "<Esc>", ":noh <CR>", { desc = "clear highlights" })

-- Remap for dealing with word wrap
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Diagnostic keymaps
vim.keymap.set("n", "gp", vim.diagnostic.goto_prev, { desc = "go to previous diagnostic message" })
vim.keymap.set("n", "gn", vim.diagnostic.goto_next, { desc = "go to next diagnostic message" })
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "open floating diagnostic message" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "open diagnostics list" })

--- VISUAL MODE KEYBINDS ---
vim.keymap.set("v", "<leader>j", "dp", { desc = "move selected lines down" })
vim.keymap.set("v", "<leader>k", "dkP", { desc = "move selected lines up" })

--- INSERT MODE KEYBINDS ---
vim.keymap.set("i", "<C-h>", "<Left>", { desc = "move left" })
vim.keymap.set("i", "<C-j>", "<Down>", { desc = "move down" })
vim.keymap.set("i", "<C-k>", "<Up>", { desc = "move up" })
vim.keymap.set("i", "<C-l>", "<Right>", { desc = "move right" })
