local ks = vim.keymap.set
local ks_opts = { noremap = true, silent = true, buffer = 0 }
local fugitive_group = vim.api.nvim_create_augroup("FugitiveCustom", {})

ks("n", "<leader>gt", ":G<CR>", { silent = true })

vim.api.nvim_create_autocmd("FileType", {
  group = fugitive_group,
  pattern = "fugitive",
  callback = function()

    vim.keymap.del("n", "p", { buffer = 0 })
    vim.keymap.del("n", "P", { buffer = 0 })

    ks("n", "q", ":close<CR>", ks_opts)
    ks("n", "pu", ":echo 'git push' | Git push<CR>", ks_opts)
    ks("n", "Pu", ":echo 'git pull' | Git pull<CR>", ks_opts)
    -- ks("n", "<Tab>", ":<C-U>execute StageInline('toggle',line('.'),v:count)<CR>", ks_opts)
  end,
})
