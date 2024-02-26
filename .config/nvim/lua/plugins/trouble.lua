-- Pretty UI for diagnostics
return {
  "folke/trouble.nvim",

  config = function()
    require("trouble").setup({ action_keys = { cancel = {} } })
  end,

  keys = { { "<leader>t", ":Trouble<CR>", desc = "Open trouble split", silent = true } },

  cmd = "Trouble",
}
