---@type PluginSpec
return {
  spec = {
    src = "https://github.com/norcalli/nvim-colorizer.lua",
  },
  setup = {
    setup = function()
      -- TODO: lazy
      require("colorizer").setup()
    end,
  },
}
