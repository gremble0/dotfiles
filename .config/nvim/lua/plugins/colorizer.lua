---@type PluginSpec
return {
  vim_pack_spec = {
    src = "https://github.com/norcalli/nvim-colorizer.lua",
  },
  setup = function()
    require("colorizer").setup()
  end,
}
