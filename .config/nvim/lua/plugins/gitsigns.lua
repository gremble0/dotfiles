-- Adds git releated signs to the gutter, as well as utilities for managing changes
---@type PluginSpec
return {
  spec = { src = "https://github.com/lewis6991/gitsigns.nvim" },
  setup = {
    setup = function()
      local gitsigns = require("gitsigns")
      gitsigns.setup({
        signs = {
          add = { text = "┃" },
          change = { text = "┃" },
          delete = { text = "┃" },
          topdelete = { text = "┃" },
          changedelete = { text = "┃" },
        },
      })

      local nav_opts = { preview = true }

      vim.keymap.set("n", "[g", function()
        gitsigns.nav_hunk("prev", nav_opts)
      end, { desc = "Goto previous git hunk" })

      vim.keymap.set("n", "]g", function()
        gitsigns.nav_hunk("next", nav_opts)
      end, { desc = "Goto next git hunk" })

      vim.keymap.set("n", "<leader>gb", function()
        gitsigns.blame_line({ full = true })
      end, { desc = "Git blame current line" })

      vim.keymap.set("n", "<leader>gv", gitsigns.preview_hunk_inline, { desc = "Preview git hunk" })

      vim.keymap.set("n", "<leader>gq", gitsigns.setqflist, { desc = "Preview git hunk" })
    end,
  },
}
