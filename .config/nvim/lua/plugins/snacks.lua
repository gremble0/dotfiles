---@type ExtendedPackSpec
return {
  src = "https://github.com/folke/snacks.nvim",
  setup = function()
    require("snacks").setup({
      picker = { enabled = true },
      input = { enabled = true, icon = "" },
      bigfile = { enabled = true },
    })
  end,
}
