---@type PluginSpec
return {
  spec = { src = "https://github.com/gremble0/gline.nvim" },
  dependencies = { { src = "https://github.com/nvim-tree/nvim-web-devicons" } },
  ---TODO: events?
  -- event = "TabNew",
  ---TODO: local dev? low prio
  -- dir = "~/Code/plugins/gline.nvim",
  setup = {
    setup = function()
      require("gline").setup()
    end,
  },
}
