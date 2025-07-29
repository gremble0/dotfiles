---@type ExtendedPackSpec
return {
  src = "https://github.com/folke/persistence.nvim",
  setup = function()
    require("persistence").setup()
  end,
}
