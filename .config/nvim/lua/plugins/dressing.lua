---@type PluginSpec
return {
  spec = { src = "https://github.com/stevearc/dressing.nvim" },
  setup = {
    setup = function()
      require("dressing").setup()
    end,
  },
}
