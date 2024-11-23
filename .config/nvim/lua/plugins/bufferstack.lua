-- Fix unintuitive :bprevious and :bnext behavior
return {
  "gremble0/bufferstack.nvim",
  dir = "~/Code/plugins/bufferstack.nvim",
  opts = {
    bprevious = "<C-p>",
    bnext = "<C-n>",
  },
}
