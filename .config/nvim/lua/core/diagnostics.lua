-- Configure diagnostic floats
vim.diagnostic.config({
  update_in_insert = true,
  float = {
    focusable = true,
    border = "rounded",
    source = true,
    prefix = "",
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
