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
})

-- Set icons for diagnostics
vim.diagnostic.config({
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = "󰅚 ",
      [vim.diagnostic.severity.WARN] = "󰀪 ",
      [vim.diagnostic.severity.INFO] = "󰋽 ",
      [vim.diagnostic.severity.HINT] = "󰌶 ",
    },
  },
})

vim.keymap.set("n", "<leader>da", vim.diagnostic.setqflist, { desc = "Add diagnostics to quickfix list" })
vim.keymap.set("n", "<leader>de", function()
  local diagnostics = vim.diagnostic.get(0)

  local errors = vim.tbl_filter(function(d)
    return d.severity == vim.diagnostic.severity.ERROR
  end, diagnostics)

  local qf_items = {}
  for _, d in ipairs(errors) do
    table.insert(qf_items, {
      bufnr = d.bufnr,
      lnum = d.lnum + 1,
      col = d.col + 1,
      text = d.message,
      type = "ERROR",
    })
  end

  vim.fn.setqflist(qf_items)
  vim.cmd("copen")
end, { desc = "Add errors to quickfix list" })
