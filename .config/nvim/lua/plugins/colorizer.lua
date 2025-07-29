---@type PluginSpec
return {
  spec = {
    src = "https://github.com/norcalli/nvim-colorizer.lua",
  },
  setup = {
    setup = function()
      require("colorizer").setup()
    end,
  },
}
