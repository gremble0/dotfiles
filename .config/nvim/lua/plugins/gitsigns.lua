-- [[ Configure gitsigns ]]
local gitsigns = require("gitsigns")

gitsigns.setup {
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "‾" },
    changedelete = { text = "~" },
  },
}

vim.keymap.set("n", "<leader>hp", gitsigns.prev_hunk, { desc = "Goto previous git hunk" })
vim.keymap.set("n", "<leader>hn", gitsigns.next_hunk, {  desc = "Goto next git hunk" })
vim.keymap.set("n", "<leader>hv", gitsigns.preview_hunk, {  desc = "Preview git hunk" })
