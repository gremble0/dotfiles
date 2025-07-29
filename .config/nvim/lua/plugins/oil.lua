-- DWIM File navigation and editing
---@type ExtendedPackSpec
return {
  src = "https://github.com/stevearc/oil.nvim",
  dependencies = { { src = "https://github.com/nvim-tree/nvim-web-devicons" } },
  setup = function()
    require("oil").setup({
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
    })

    vim.keymap.set("n", "<C-e>", function()
      require("oil").open()
    end, { desc = "Open file explorer (oil)" })
  end,
}
