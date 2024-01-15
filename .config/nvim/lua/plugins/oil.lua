-- [[ Configure oil ]]
require("oil").setup({
  columns = { "permissions", "size", "mtime", "icon" },
  use_default_keymaps = false,
  view_options = { show_hidden = true },
})

vim.keymap.set("n", "<C-e>", require("oil").open, { desc = "Open file explorer (oil)" })
