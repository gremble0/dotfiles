local oil = require("oil")

oil.setup({
  columns = { "permissions", "size", "mtime", "icon" },
  keymaps = {
    ["<CR>"] = "actions.select",
    ["-"] = "actions.parent",
  },
  use_default_keymaps = false,
  view_options = { show_hidden = true },
})
