local ks = vim.keymap.set

ks("n", "<leader>gt", ":G<CR>", { silent = true })

local fugitive_group = vim.api.nvim_create_augroup("FugitiveCustomMaps", {})
vim.api.nvim_create_autocmd("FileType", {
  group = fugitive_group,
  pattern = "fugitive",
  callback = function()
    local opts = { noremap = true, silent = true, buffer = 0 }

    vim.keymap.del("n", "p", { buffer = 0 })
    vim.keymap.del("n", "P", { buffer = 0 })

    ks("n", "q", ":close<CR>", opts)
    ks("n", "pu", ":Git push<CR>", opts)
    ks("n", "Pu", ":Git pull<CR>", opts)
    -- vim.keymap.set("n", "<Tab>", ":<C-U>execute <SNR>52_StageInline('toggle',line('.'),v:count)<CR>", opts)
  end
})
