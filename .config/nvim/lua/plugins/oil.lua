-- [[ Configure oil ]]
local com = require("core.common")

require("oil").setup({
  columns = { "permissions", "size", "mtime", "icon" },
  keymaps = {
    ["<CR>"] = "actions.select",
    ["-"] = "actions.parent",
  },
  use_default_keymaps = false,
  view_options = { show_hidden = true },
})

com.ks("n", "<C-e>", require("oil").open, { desc = "Open file explorer (oil)" })
