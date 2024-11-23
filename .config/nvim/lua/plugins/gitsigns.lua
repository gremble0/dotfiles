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
    { "<leader>gp", ":Gitsigns prev_hunk<CR>", desc = "Goto previous git hunk", silent = true },
    { "<leader>gn", ":Gitsigns next_hunk<CR>", desc = "Goto next git hunk", silent = true },
    { "<leader>gv", ":Gitsigns preview_hunk_inline<CR>", desc = "Preview git hunk", silent = true },
  },
  lazy = false,
}
