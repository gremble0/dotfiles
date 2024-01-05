-- [[ Configure gitsigns ]]
require("gitsigns").setup {
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "‾" },
    changedelete = { text = "~" },
  },
}

vim.keymap.set("n", "<leader>hp", require("gitsigns").prev_hunk, { desc = "Goto previous git hunk" })
vim.keymap.set("n", "<leader>hn", require("gitsigns").next_hunk, {  desc = "Goto next git hunk" })
vim.keymap.set("n", "<leader>hv", require("gitsigns").preview_hunk, {  desc = "Preview git hunk" })
