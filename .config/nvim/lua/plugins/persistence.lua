---@type PluginSpec
return {
  vim_pack_spec = { src = "https://github.com/folke/persistence.nvim" },
  setup = function()
    require("persistence").setup()
  end,
}
