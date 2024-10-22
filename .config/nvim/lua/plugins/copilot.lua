return {
  "zbirenbaum/copilot.lua",

  cmd = "Copilot",

  event = "InsertEnter",

  config = function()
    require("copilot").setup({
      suggestion = {
        keymap = {
          accept = "<M-CR>",
          next = "<M-n>",
          prev = "<M-p>",
          dismiss = "<M-Esc>",
        },
      },
    })
  end,
}
