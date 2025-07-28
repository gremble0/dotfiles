-- Git integration
---@type PluginSpec
return {
  spec = {
    src = "https://github.com/tpope/vim-fugitive",
  },
  dependencies = { { src = "https://github.com/junegunn/gv.vim" } },
  ---TODO: keybinds
  -- keys = {
  --   { "<leader>gt", ":vertical Git<CR>", desc = "Open fugitive", silent = true },
  --   { "<leader>gc", ":GV<CR>", desc = "Open commit history", silent = true },
  -- },
  ---TODO: lazy
  -- cmd = { "Git", "GV", "G" },
  setup = {
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
    end,
  },
}
