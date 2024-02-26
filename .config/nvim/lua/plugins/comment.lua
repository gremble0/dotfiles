-- Keybinds for commenting regions
return {
  "numToStr/Comment.nvim",

  config = function()
    require("Comment").setup({
      toggler = {
        line = "<leader>/",
        block = "<leader>?",
      },
      opleader = {
        line = "<leader>/",
        block = "<leader>?",
      },
    })
  end,
}
