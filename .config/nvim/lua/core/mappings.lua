-- [[ Keymaps ]]
--- NORMAL MODE KEYBINDS ---
vim.keymap.set({ "n", "v" }, "<leader>", "<Nop>", { desc = "combine with other keys to invoke commands", silent = true })

-- Fast navigate between windows
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "move to window to the left" })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "move to window below" })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "move to window above" })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "move to window to the right" })

-- Buffer navigation
-- vim.keymap.set("n", "<C-p>", ":bprev<CR>", { desc = "go to previous buffer", silent = true })
-- vim.keymap.set("n", "<C-n>", ":bnext<CR>", { desc = "go to next buffer", silent = true })
vim.keymap.set("n", "<C-c>", "<C-w>c", { desc = "close buffer", silent = true })

-- Move current line up or down 
vim.keymap.set("n", "<leader>j", ":m .+1<CR>", { desc = "move current line down", silent = true })
vim.keymap.set("n", "<leader>k", ":m .-2<CR>", { desc = "move current line up", silent = true })

-- Clear highlights with escape
vim.keymap.set("n", "<Esc>", ":noh<CR>", { desc = "clear highlights" })

-- Remap for dealing with word wrap
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Open netrw
vim.keymap.set("n", "<C-e>", ":Ex<CR>", { desc = "open netrw" })

local open_float_opts = {
  focusable = false,
  close_events = { "BufLeave", "CursorMoved", "InsertEnter", "FocusLost" },
  border = "single",
  source = "always", -- show source in diagnostic popup window
  prefix = " ",
}

local open_float = function() vim.diagnostic.open_float(open_float_opts) end
local goto_next = function() vim.diagnostic.goto_next({ float = open_float_opts }) end
local goto_prev = function() vim.diagnostic.goto_prev({ float = open_float_opts }) end

-- Diagnostic keymaps
vim.keymap.set("n", "gp", goto_prev, { desc = "go to previous diagnostic message" })
vim.keymap.set("n", "gn", goto_next, { desc = "go to next diagnostic message" })
vim.keymap.set("n", "<leader>e", open_float, { desc = "open floating diagnostic message" })

--- VISUAL MODE KEYBINDS ---
vim.keymap.set("v", "<leader>j", "dp", { desc = "move selected lines down" })
vim.keymap.set("v", "<leader>k", "dkP", { desc = "move selected lines up" })

--- INSERT MODE KEYBINDS ---
vim.keymap.set("i", "<C-h>", "<Left>", { desc = "move left" })
vim.keymap.set("i", "<C-j>", "<Down>", { desc = "move down" })
vim.keymap.set("i", "<C-k>", "<Up>", { desc = "move up" })
vim.keymap.set("i", "<C-l>", "<Right>", { desc = "move right" })
