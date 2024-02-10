local nvim_tree = require("nvim-tree")
local com = require("core.common")

nvim_tree.setup({
  disable_netrw = true,
  hijack_netrw = true,
  git = {
    enable = false,
  },
  filesystem_watchers = {
    enable = true,
  },
  actions = {
    open_file = {
      resize_window = true,
    },
  },
  renderer = {
    root_folder_label = false,

    icons = {
      show = {
        folder_arrow = true,
        git = false,
      },

      glyphs = {
        default = "",
        symlink = "",
        folder = {
          default = "",
          empty = "",
          empty_open = "",
          open = "",
          symlink = "",
          symlink_open = "",
          arrow_open = "",
          arrow_closed = "",
        },
      },
    },
  },
})

com.ks("n", "<C-n>", ":NvimTreeToggle<CR>", { desc = "Toggle nvim-tree", silent = true })
