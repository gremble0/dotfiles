return {
  "theprimeagen/harpoon",

  branch = "harpoon2",

  dependencies = "nvim-lua/plenary.nvim",

  keys = {
    {
      "<leader>hm",
      ":lua require('harpoon').ui:toggle_quick_menu(require('harpoon'):list())<CR>",
      desc = "Toggle harpoon quick menu",
      silent = true,
    },
    {
      "<leader>ha",
      ":lua require('harpoon'):list():append()<CR>",
      desc = "Append buffer to harpoon list",
      silent = true,
    },

    { "<M-1>", ":lua require('harpoon'):list():select(1)<CR>", desc = "Select first harpoon entry", silent = true },
    { "<M-2>", ":lua require('harpoon'):list():select(2)<CR>", desc = "Select second harpoon entry", silent = true },
    { "<M-3>", ":lua require('harpoon'):list():select(3)<CR>", desc = "Select third harpoon entry", silent = true },
    { "<M-4>", ":lua require('harpoon'):list():select(4)<CR>", desc = "Select fourth harpoon entry", silent = true },
    { "<M-5>", ":lua require('harpoon'):list():select(5)<CR>", desc = "Select fifth harpoon entry", silent = true },
    { "<M-6>", ":lua require('harpoon'):list():select(6)<CR>", desc = "Select sixth harpoon entry", silent = true },
    { "<M-7>", ":lua require('harpoon'):list():select(7)<CR>", desc = "Select seventh harpoon entry", silent = true },
    { "<M-8>", ":lua require('harpoon'):list():select(8)<CR>", desc = "Select eighth harpoon entry", silent = true },
    { "<M-9>", ":lua require('harpoon'):list():select(9)<CR>", desc = "Select ninth harpoon entry", silent = true },
  },
}
