-- Fix unintuitive :bprevious and :bnext behavior
---@type PluginSpec
return {
  vim_pack_spec = {
    src = "https://github.com/gremble0/bufferstack.nvim",
  },
  setup = function()
    require("bufferstack").setup({
      bprevious = "[b",
      bnext = "]b",
    })
  end,
}
