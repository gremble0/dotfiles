---@type PluginSpec
return {
  spec = { src = "https://github.com/gremble0/gline.nvim" },
  dependencies = { { spec = { src = "https://github.com/nvim-tree/nvim-web-devicons" } } },
  ---TODO: local dev? low prio
  -- dir = "~/Code/plugins/gline.nvim",
  setup = {
    setup = function()
      require("gline").setup()
    end,
  },
}
