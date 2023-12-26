local killring = require("killring")

killring.setup {
  buffer_local = true,
}

vim.keymap.set("n", "<leader>gk", killring.open)
