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
      is_always_hidden = function(name, _)
        return name == ".."
      end,
    },
  },

  -- Check if vim is opened on a directory (won't be handled by plugin since its lazy loaded)
  init = function(oil)
    if vim.fn.isdirectory(vim.fn.expand("%")) == 1 then
      require("oil").setup(oil.opts)
    end
  end,

  keys = {
    { "<C-e>", ":Oil<CR>", desc = "Open file explorer (oil)", silent = true },
  },

  cmd = "Oil",
}
