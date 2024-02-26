-- Fix unintuitive :bprevious and :bnext behavior
return {
  "gremble0/bufferstack.nvim",

  config = function()
    local bufferstack = require("bufferstack")

    bufferstack.setup({
      bprevious = "<C-p>",
      bnext = "<C-n>",
    })
  end,
}
