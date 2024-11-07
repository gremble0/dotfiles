vim.keymap.set(
  "n",
  "gs",
  ":ClangdSwitchSourceHeader<CR>",
  { desc = "Clang toggle between source and header file", silent = true }
)
