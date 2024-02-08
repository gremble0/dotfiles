local fugitive_group = vim.api.nvim_create_augroup("FugitiveCustomMaps", {})

vim.api.nvim_create_autocmd("FileType", {
  group = fugitive_group,
  pattern = "fugitive",
  callback = function()
    local opts = { noremap = true, silent = true, buffer = 0 }
    vim.keymap.set("n", "q", ":close<CR>", opts)
    -- vim.keymap.set("n", "<Tab>", ":<C-U>execute <SNR>52_StageInline('toggle',line('.'),v:count)<CR>", opts)
  end
})
