local ks = vim.keymap.set

--- NORMAL MODE KEYBINDS
-- Faster navigation between windows
ks("n", "<C-h>", "<C-w>h", { desc = "Move to window to the left" })
ks("n", "<C-j>", "<C-w>j", { desc = "Move to window below" })
ks("n", "<C-k>", "<C-w>k", { desc = "Move to window above" })
ks("n", "<C-l>", "<C-w>l", { desc = "Move to window to the right" })

-- Buffers
ks("n", "<C-c>", ":close!<CR>", { desc = "Close buffer", silent = true })
ks("n", "<C-q>", "<C-^>", { desc = "Alternate file" })

-- Tabs
ks("n", "<leader>tn", ":tabnew<CR>", { desc = "Make new tab", silent = true })
ks("n", "<leader>tc", ":tabclose<CR>", { desc = "Close tab", silent = true })

-- Quickfix-/Location- list
ks("n", "<leader>qo", ":copen<CR>", { desc = "Open quickfix list", silent = true })
ks("n", "<leader>qc", ":cclose<CR>", { desc = "Close quickfix list", silent = true })
ks("n", "<leader>l", ":lopen<CR>", { desc = "Open location list", silent = true })
ks("n", "<leader>qp", ":cclose<CR>:cp<CR>", { desc = "Go to previous quickfix error", silent = true })
ks("n", "<leader>qn", ":cclose<CR>:cn<CR>", { desc = "Go to next quickfix error", silent = true })

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

-- Copy stuff from current file (useful for debugging with gdb)
ks("n", "<leader>cp", function()
  vim.fn.system(string.format("echo %s | xclip -selection clipboard", vim.fn.expand("%")))
end, {})
ks("n", "<leader>cl", function()
  vim.fn.system(string.format("echo %s:%s | xclip -selection clipboard", vim.fn.expand("%"), vim.fn.line(".")))
end, {})

--- VISUAL MODE KEYBINDS
-- Move lines
ks("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selected lines down", silent = true })
ks("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selected lines up", silent = true })

--- INSERT AND COMMAND MODE KEYBINDS
-- Navigation
ks({ "i", "c" }, "<C-h>", "<Left>", { desc = "Move left" })
ks({ "i", "c" }, "<C-j>", "<Down>", { desc = "Move down" })
ks({ "i", "c" }, "<C-k>", "<Up>", { desc = "Move up" })
ks({ "i", "c" }, "<C-l>", "<Right>", { desc = "Move right" })
ks({ "i", "c" }, "<C-a>", "<Home>", { desc = "Move to the start of the line" })
ks({ "i", "c" }, "<C-e>", "<End>", { desc = "Move to the end of the line" })
ks({ "i", "c" }, "<M-f>", "<C-Right>", { desc = "Move forward word" })
ks({ "i", "c" }, "<M-b>", "<C-Left>", { desc = "Move back word" })
ks({ "i", "c" }, "<C-b>", "<Left>", { desc = "Move left" })
ks({ "i", "c" }, "<C-f>", "<Right>", { desc = "Move right" })
