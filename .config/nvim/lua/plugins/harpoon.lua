return {
  "theprimeagen/harpoon",

  branch = "harpoon2",

  dependencies = "nvim-lua/plenary.nvim",

  config = function()
    local ks = vim.keymap.set
    local harpoon = require("harpoon")

    harpoon:setup({})

    ks("n", "<leader>h", function()
      harpoon.ui:toggle_quick_menu(harpoon:list())
    end)
    ks("n", "<leader>a", function()
      harpoon:list():append()
    end)

    for i = 1, 9 do
      ks("n", "<M-" .. i .. ">", function()
        harpoon:list():select(i)
      end)
    end
  end,
}
