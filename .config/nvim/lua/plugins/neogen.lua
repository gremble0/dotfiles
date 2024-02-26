-- Generate documentation
return {
  "danymat/neogen",

  dependencies = "nvim-treesitter/nvim-treesitter",

  opts = { snippet_engine = "luasnip" },

  keys = {
    { "<leader>gd", ":Neogen<CR>", desc = "Generate documentation", silent = true },
  },

  cmd = "Neogen",
}
