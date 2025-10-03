vim.keymap.set(
  "n",
  "gs",
  ":LspClangdSwitchSourceHeader<CR>",
  { desc = "Clang toggle between source and header file", silent = true }
)
