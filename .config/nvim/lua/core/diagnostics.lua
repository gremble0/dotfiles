-- Configure diagnostic floats
vim.diagnostic.config({
  update_in_insert = true,
  float = {
    focusable = true,
    border = "rounded",
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
    float = true
  },
  virtual_text = true,
})

vim.keymap.set("n", "<leader>da", vim.diagnostic.setqflist, { desc = "Add diagnostics to quickfix list" })
vim.keymap.set("n", "<leader>de", function()
  local diagnostics = vim.diagnostic.get(0)

  local errors = vim.tbl_filter(function(diagnostic)
    return diagnostic.severity == vim.diagnostic.severity.ERROR
  end, diagnostics)

  ---@type vim.quickfix.entry[]
  local qf_items = {}
  for _, diagnostic in ipairs(errors) do
    table.insert(qf_items, {
      bufnr = diagnostic.bufnr,
      lnum = diagnostic.lnum + 1,
      col = diagnostic.col + 1,
      text = diagnostic.message,
      type = "ERROR",
    })
  end

  vim.fn.setqflist(qf_items)
  vim.cmd("copen")
end, { desc = "Add errors to quickfix list" })

vim.lsp.set_log_level("OFF")
