-- [[ Configure gitsigns ]]
local gitsigns = require("gitsigns")

gitsigns.setup({
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "‾" },
    changedelete = { text = "~" },
  },
})

local ks = vim.keymap.set
ks("n", "<leader>hp", gitsigns.prev_hunk, { desc = "Goto previous git hunk" })
ks("n", "<leader>hn", gitsigns.next_hunk, {  desc = "Goto next git hunk" })
ks("n", "<leader>hv", gitsigns.preview_hunk, {  desc = "Preview git hunk" })
