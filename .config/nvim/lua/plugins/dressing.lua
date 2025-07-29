---@type PluginSpec
return {
  vim_pack_spec = { src = "https://github.com/stevearc/dressing.nvim" },
  setup = function()
    require("dressing").setup()
  end,
}
