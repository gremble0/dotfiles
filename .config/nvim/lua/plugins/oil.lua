-- DWIM File navigation and editing
return {
  "stevearc/oil.nvim",

  dependencies = "nvim-tree/nvim-web-devicons",

  opts = {
    columns = { "permissions", "size", "mtime", "icon" },
    win_options = { signcolumn = "yes" },
    keymaps = {
      ["<CR>"] = "actions.select",
      ["-"] = "actions.parent",
      [">"] = "actions.preview",
    },
    use_default_keymaps = false,
    view_options = {
      show_hidden = true,
      -- Hide parent dir
      ---@param name string
      ---@param _ integer
      is_always_hidden = function(name, _)
        return name == ".."
      end,
    },
  },

  keys = {
    { "<C-e>", ":Oil<CR>", desc = "Open file explorer (oil)", silent = true },
  },

  cmd = "Oil",
}
