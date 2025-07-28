---@type PluginSpec
return {
  spec = { src = "https://github.com/folke/persistence.nvim" },
  ---TODO: event
  -- event = "BufReadPre",
  setup = {
    setup = function()
      require("persistence").setup()
    end,
  },
}
