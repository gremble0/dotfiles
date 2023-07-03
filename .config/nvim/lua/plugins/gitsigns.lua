-- [[ Configure gitsigns ]]
require("gitsigns").setup {
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "‾" },
    changedelete = { text = "~" },
  },

  vim.keymap.set("n", "<leader>gp", require("gitsigns").prev_hunk, { desc = "[G]o to [P]revious Hunk" }),
  vim.keymap.set("n", "<leader>gn", require("gitsigns").next_hunk, {  desc = "[G]o to [N]ext Hunk" }),
  vim.keymap.set("n", "<leader>ph", require("gitsigns").preview_hunk, {  desc = "[P]review [H]unk" }),
}
