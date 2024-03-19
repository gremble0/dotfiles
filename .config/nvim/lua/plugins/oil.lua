-- DWIM File navigation and editing
return {
  "stevearc/oil.nvim",

  dependencies = "nvim-tree/nvim-web-devicons",

  opts = {
    columns = { "permissions", "size", "mtime", "icon" },
    keymaps = {
      ["<CR>"] = "actions.select",
      ["-"] = "actions.parent",
    },
    use_default_keymaps = false,
    view_options = { show_hidden = true },
  },

  keys = {
    { "<C-e>", ":Oil<CR>", desc = "Open file explorer (oil)", silent = true },
  },

  cmd = "Oil",
}
