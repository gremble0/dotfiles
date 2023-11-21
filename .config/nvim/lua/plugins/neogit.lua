-- [[ Configure Neogit ]]
local neogit = require("neogit")

neogit.setup {
  vim.keymap.set("n", "<leader>gt", neogit.open, { desc = "Toggle nvim-tree", silent = true }),

  kind = "vsplit",
}
