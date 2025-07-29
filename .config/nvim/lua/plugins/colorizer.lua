---@type ExtendedPackSpec
return {
  src = "https://github.com/norcalli/nvim-colorizer.lua",
  setup = function()
    require("colorizer").setup()
  end,
}
