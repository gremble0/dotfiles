local ks = vim.keymap.set

ks("n", "<C-p>", ":cp<CR>:copen<CR>", { desc = "Go to previous quickfix error", silent = true })
ks("n", "<C-n>", ":cn<CR>:copen<CR>", { desc = "Go to next quickfix error", silent = true })
