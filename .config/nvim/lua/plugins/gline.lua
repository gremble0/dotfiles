---@type ExtendedPackSpec
return {
  src = "https://github.com/gremble0/gline.nvim",
  dependencies = { { src = "https://github.com/nvim-tree/nvim-web-devicons" } },
  setup = function()
    require("gline").setup()
  end,
}
