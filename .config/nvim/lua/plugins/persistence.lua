---@type PluginSpec
return {
  spec = { src = "https://github.com/folke/persistence.nvim" },
  setup = {
    setup = function()
      require("persistence").setup()
    end,
  },
}
