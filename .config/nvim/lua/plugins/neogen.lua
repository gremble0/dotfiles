local neogen = require("neogen")
neogen.setup({ snippet_engine = "luasnip" })
vim.keymap.set("n", "<leader>gd", neogen.generate, { desc = "[G]enerate [D]ocumentation" })
