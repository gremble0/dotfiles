---@type PluginSpec
return {
  vim_pack_spec = { src = "https://github.com/gremble0/gline.nvim" },
  dependencies = { { vim_pack_spec = { src = "https://github.com/nvim-tree/nvim-web-devicons" } } },
  ---TODO: local dev? low prio
  -- dir = "~/Code/plugins/gline.nvim",
  setup = function()
    require("gline").setup()
  end,
}
