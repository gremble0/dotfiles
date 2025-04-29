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
      "[g",
      function()
        require("gitsigns").nav_hunk("prev")
      end,
      desc = "Goto previous git hunk",
    },
    {
      "]g",
      function()
        require("gitsigns").nav_hunk("next")
      end,
      desc = "Goto next git hunk",
    },
    {
      "<leader>gb",
      function()
        require("gitsigns").blame_line({ full = true })
      end,
      desc = "Git blame current line",
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
