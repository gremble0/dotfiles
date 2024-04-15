local ks = vim.keymap.set

local buf = vim.api.nvim_get_current_buf()

ks("n", "<C-p>", ":cp<CR>:copen<CR>", { desc = "Go to previous quickfix error", silent = true, buffer = buf })
ks("n", "<C-n>", ":cn<CR>:copen<CR>", { desc = "Go to next quickfix error", silent = true, buffer = buf })
