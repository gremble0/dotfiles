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
local icons = {
  Error = "󰅚 ",
  Warn = "󰀪 ",
  Info = "󰋽 ",
  Hint = "󰌶 ",
}

for type, icon in pairs(icons) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

vim.keymap.set("n", "<leader>d", vim.diagnostic.setqflist, { desc = "Add diagnostics to quickfix list" })
vim.keymap.set("n", "<leader>d", vim.diagnostic.setqflist, { desc = "Add diagnostics to quickfix list" })
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
