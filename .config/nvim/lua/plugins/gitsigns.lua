local gitsigns = require("gitsigns")
local com = require("core.common")

gitsigns.setup({
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "‾" },
    changedelete = { text = "~" },
  },
})

com.ks("n", "<leader>hp", gitsigns.prev_hunk, { desc = "Goto previous git hunk" })
com.ks("n", "<leader>hn", gitsigns.next_hunk, {  desc = "Goto next git hunk" })
com.ks("n", "<leader>hv", gitsigns.preview_hunk, {  desc = "Preview git hunk" })
