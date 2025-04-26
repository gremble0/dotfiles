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

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(e)
    local client = vim.lsp.get_client_by_id(e.data.client_id)
    if client and client:supports_method("textDocument/completion") then
      vim.lsp.completion.enable(true, client.id, e.buf)
    end
  end
})
