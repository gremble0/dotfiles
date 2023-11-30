-- [[ Configure ibl (indent-blankline.nvim) ]]
require("ibl").setup {
  indent = {
    highlight = { "VertSplit" },
    char = "▏",
  },
  scope = {
    highlight = { "Comment" },
    char = "▏",
    show_start = false,
  },
}
