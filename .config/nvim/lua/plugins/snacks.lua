---@type ExtendedPackSpec
return {
  src = "https://github.com/folke/snacks.nvim",
  setup = function()
    require("snacks").setup({
      picker = { enabled = true },
      bigfile = { enabled = true },
    })
  end,
}
