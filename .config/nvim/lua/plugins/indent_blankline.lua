-- [[ Configure Indent Blankline ]]
require("indent_blankline").setup {
  char = "│",
  show_trailing_blankline_indent = false,
  vim.cmd.highlight "IndentBlanklineChar guifg=#333333 gui=nocombine"
}
