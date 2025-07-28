-- Fix unintuitive :bprevious and :bnext behavior
---@type PluginSpec
return {
  spec = {
    src = "https://github.com/gremble0/bufferstack.nvim",
  },
  setup = {
    setup = function()
      require("bufferstack").setup({
        bprevious = "[b",
        bnext = "]b",
      })
    end,
  },
}
