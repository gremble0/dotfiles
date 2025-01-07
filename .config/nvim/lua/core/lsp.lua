local lsp = vim.lsp
local ks = vim.keymap.set

lsp.handlers["textDocument/hover"] = lsp.with(lsp.handlers.hover, { border = "rounded" })
lsp.handlers["textDocument/signatureHelp"] = lsp.with(lsp.handlers.signature_help, { border = "rounded" })

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("LspGroup", {}),
  callback = function(e)
    ks("n", "<leader>rn", lsp.buf.rename, { buffer = e.buf, desc = "LSP: Rename" })
    ks("n", "<leader>ca", lsp.buf.code_action, { buffer = e.buf, desc = "LSP: Code action" })

    ks("n", "gd", lsp.buf.definition, { buffer = e.buf, desc = "LSP: Goto definition" })
    ks("n", "gD", lsp.buf.declaration, { buffer = e.buf, desc = "LSP: Goto declaration" })
    ks("n", "gi", lsp.buf.implementation, { buffer = e.buf, desc = "LSP: Goto implementation" })
    ks("n", "gr", lsp.buf.references, { buffer = e.buf, desc = "LSP: Goto references" })

    ks("n", "K", lsp.buf.hover, { buffer = e.buf, desc = "LSP: Hover documentation" })
    ks({ "i", "s" }, "<C-s>", function()
      local ok, cmp = pcall(require, "cmp")
      if ok then
        cmp.close()
      end
      lsp.buf.signature_help()
    end, { buffer = e.buf, desc = "LSP: Signature Documentation" })
  end,
})
