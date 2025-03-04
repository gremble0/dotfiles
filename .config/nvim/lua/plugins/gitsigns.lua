-- Adds git releated signs to the gutter, as well as utilities for managing changes
return {
  "lewis6991/gitsigns.nvim",
  opts = {
    signs = {
      add = { text = "┃" },
      change = { text = "┃" },
      delete = { text = "┃" },
      topdelete = { text = "┃" },
      changedelete = { text = "┃" },
    },
  },
  keys = {
    {
      "<leader>gp",
      function()
        require("gitsigns").nav_hunk("prev")
      end,
      desc = "Goto previous git hunk",
    },
    {
      "<leader>gn",
      function()
        require("gitsigns").nav_hunk("next")
      end,
      desc = "Goto next git hunk",
    },
    {
      "<leader>gv",
      function()
        require("gitsigns").preview_hunk_inline()
      end,
      desc = "Preview git hunk",
    },
  },
  lazy = false,
}
