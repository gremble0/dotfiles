local ks = vim.keymap.set

--- NORMAL MODE KEYBINDS
-- Faster navigation between windows
ks("n", "<C-h>", "<C-w>h", { desc = "Move to window to the left" })
ks("n", "<C-j>", "<C-w>j", { desc = "Move to window below" })
ks("n", "<C-k>", "<C-w>k", { desc = "Move to window above" })
ks("n", "<C-l>", "<C-w>l", { desc = "Move to window to the right" })

-- Resize buffers
ks("n", "<C-S-H>", "<C-w>5<", { desc = "Shrink current split horizontally" })
ks("n", "<C-S-J>", "<C-w>-", { desc = "Shrink current split vertically" })
ks("n", "<C-S-K>", "<C-w>+", { desc = "Grow current split vertically" })
ks("n", "<C-S-L>", "<C-w>5>", { desc = "Grow current split horizontally" })

-- Buffers
ks("n", "<C-c>", ":close!<CR>", { desc = "Close buffer", silent = true })
ks("n", "<C-q>", "<C-^>", { desc = "Alternate file" })

-- Quickfix-/Location- list
ks("n", "<leader>q", ":copen<CR>", { desc = "Open quickfix list", silent = true })
ks("n", "<leader>l", ":lopen<CR>", { desc = "Open location list", silent = true })
ks("n", "<C-S-P>", ":cclose<CR>:cp<CR>", { desc = "Go to previous quickfix error", silent = true })
ks("n", "<C-S-N>", ":cclose<CR>:cn<CR>", { desc = "Go to next quickfix error", silent = true })

-- Clear highlights with escape
ks("n", "<Esc>", ":noh<CR>", { desc = "Clear highlights" })

-- Remap for dealing with word wrap
ks("n", "k", "v:count == 0 ? 'gk' : 'k'", { desc = "Move up with word wrapping", expr = true, silent = true })
ks("n", "j", "v:count == 0 ? 'gj' : 'j'", { desc = "Move down with word wrapping", expr = true, silent = true })

-- Diagnostic keymaps
ks("n", "gp", vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })
ks("n", "gn", vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })
ks("n", "<leader>e", vim.diagnostic.open_float, { desc = "Open floating diagnostic message" })

-- Open lazy
ks("n", "<leader>z", ":Lazy<CR>", { desc = "Open lazy", silent = true })

--- VISUAL MODE KEYBINDS
ks("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selected lines down", silent = true })
ks("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selected lines up", silent = true })

--- INSERT MODE KEYBINDS
ks("i", "<C-h>", "<Left>", { desc = "Move left" })
ks("i", "<C-j>", "<Down>", { desc = "Move down" })
ks("i", "<C-k>", "<Up>", { desc = "Move up" })
ks("i", "<C-l>", "<Right>", { desc = "Move right" })
ks("i", "<C-a>", "<Home>", { desc = "Move to the start of the line" })
ks("i", "<C-e>", "<End>", { desc = "Move to the end of the line" })

-- COMMAND MODE KEYBINDS
ks("c", "<C-b>", "<Left>", { desc = "Move left" })
ks("c", "<C-f>", "<Right>", { desc = "Move right" })
ks("c", "<C-a>", "<Home>", { desc = "Move to the start of the ex prompt" })
ks("c", "<C-e>", "<End>", { desc = "Move to the end of the ex prompt" })
