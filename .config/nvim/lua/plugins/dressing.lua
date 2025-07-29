---@type ExtendedPackSpec
return {
  src = "https://github.com/stevearc/dressing.nvim",
  setup = function()
    require("dressing").setup()
  end,
}
