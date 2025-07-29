-- Git integration
---@type PluginSpec
return {
  vim_pack_spec = {
    src = "https://github.com/tpope/vim-fugitive",
  },
  dependencies = { { vim_pack_spec = { src = "https://github.com/junegunn/gv.vim" } } },
  setup = function()
    vim.api.nvim_create_autocmd("FileType", {
      group = vim.api.nvim_create_augroup("FugitiveCustom", {}),
      pattern = "fugitive",
      callback = function(e)
        vim.keymap.del("n", "p", { buffer = e.buf })

        vim.keymap.set("n", "pu", ":Git push<CR>", { desc = "Git push", silent = true, buffer = e.buf })
        vim.keymap.set("n", "pl", ":Git pull<CR>", { desc = "Git pull", silent = true, buffer = e.buf })
      end,
    })

    vim.keymap.set("n", "<leader>gt", ":vertical Git<CR>", { desc = "Open fugitive", silent = true })
    vim.keymap.set("n", "<leader>gc", ":GV<CR>", { desc = "Open commit history", silent = true })
  end,
}
