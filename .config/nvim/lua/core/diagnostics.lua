-- Configure diagnostic floats
vim.diagnostic.config({
  update_in_insert = true,
  float = {
    focusable = false,
    border = "rounded",
    source = "always",
    prefix = "",
  },
})

local icons = {
  Error = "󰅚 ",
  Warn = "󰀪 ",
  Info = "󰋽 ",
  Hint = "󰌶 ",
}

-- Set icons for diagnostics
for type, icon in pairs(icons) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end
