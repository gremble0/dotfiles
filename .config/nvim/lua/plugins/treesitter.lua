-- Highlight, edit, and navigate code
---@type ExtendedPackSpec
return {
  src = "https://github.com/nvim-treesitter/nvim-treesitter",
  setup = function()
    require("nvim-treesitter.configs").setup({
      ensure_installed = {},
      sync_install = false,
      auto_install = true,
      ignore_install = {},
      highlight = { enable = true },
      indent = { enable = true },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "v<Tab>",
          node_incremental = "<Tab>",
          node_decremental = "<S-Tab>",
        },
      },
      modules = {},
    })
  end,
  build = function()
    require("nvim-treesitter.install").update()
  end,
}
