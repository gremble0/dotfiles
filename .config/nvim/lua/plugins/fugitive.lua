-- Git integration
return {
  "tpope/vim-fugitive",

  keys = { { "<leader>gt", ":vertical Git<CR>", desc = "Open fugitive", silent = true } },

  cmd = "Git",

  config = function()
    local fugitive_group = vim.api.nvim_create_augroup("FugitiveCustom", {})

    local kd = vim.keymap.del
    local ks = vim.keymap.set

    vim.api.nvim_create_autocmd("FileType", {
      group = fugitive_group,
      pattern = "fugitive",
      callback = function()
        kd("n", "p", { buffer = 0 })
        kd("n", "P", { buffer = 0 })

        ks("n", "pu", ":Git push<CR>", { desc = "Git push", silent = true, buffer = 0 })
        ks("n", "Pu", ":Git pull<CR>", { desc = "Git pull", silent = true, buffer = 0 })
      end,
    })
  end,
}
