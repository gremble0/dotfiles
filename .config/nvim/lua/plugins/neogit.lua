-- [[ Configure Neogit ]]
local neogit = require("neogit")

neogit.setup {
  vim.keymap.set("n", "<leader>gt", neogit.open, { desc = "Open neogit" }),

  disable_hint = true,
  kind = "vsplit",
  mappings = {
    popup = {
      ["p"] = "PushPopup",
      ["P"] = "PullPopup",
    }
  }
}
