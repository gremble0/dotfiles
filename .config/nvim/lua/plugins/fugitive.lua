-- Git integration
return {
  "tpope/vim-fugitive",

  keys = { { "<leader>gt", ":vertical Git<CR>", desc = "Open fugitive", silent = true } },

  cmd = "Git",

  config = function()
    vim.api.nvim_create_autocmd("FileType", {
      group = vim.api.nvim_create_augroup("FugitiveCustom", {}),
      pattern = "fugitive",
      callback = function(e)
        vim.keymap.del("n", "p", { buffer = e.buf })
        vim.keymap.del("n", "P", { buffer = e.buf })

        vim.keymap.set("n", "pu", ":Git push<CR>", { desc = "Git push", silent = true, buffer = e.buf })
        vim.keymap.set("n", "Pu", ":Git pull<CR>", { desc = "Git pull", silent = true, buffer = e.buf })
      end,
    })
  end,
}
