-- Some language servers spam logs too much making nvim generate warnings for having large log files.
-- I never look at these anyways, so just disable them
vim.lsp.log.set_level(vim.log.levels.OFF)

-- Configure diagnostic floats
vim.diagnostic.config({
  update_in_insert = true,
  float = {
    focusable = true,
    source = true,
    prefix = "",
    header = { "Diagnostics: ", "@markup.heading" },
  },
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = "󰅚 ",
      [vim.diagnostic.severity.WARN] = "󰀪 ",
      [vim.diagnostic.severity.INFO] = "󰋽 ",
      [vim.diagnostic.severity.HINT] = "󰌶 ",
    },
  },
  jump = {
    float = true,
  },
  virtual_text = true,
})

vim.keymap.set("n", "<leader>dq", vim.diagnostic.setqflist, { desc = "Add all known diagnostics to quickfix list" })

vim.keymap.set("n", "<leader>de", function()
  vim.diagnostic.setloclist({ severity = 1 })
end, { desc = "Add diagnostics for all errors in the current buffer to quickfix list" })

vim.keymap.set(
  "n",
  "<leader>dl",
  vim.diagnostic.setloclist,
  { desc = "Add diagnostics for the current buffer to quickfix list" }
)
