-- [[ Configures native lsp and integrates nvim-cmp and mason to apply language servers ]]
local lsp = vim.lsp
local ks = vim.keymap.set

-- Get user input for renaming the word currently under the cursor
local rename = function()
  local cur_name = vim.fn.expand("<cword>")

  local new_name = vim.fn.input("Rename '" .. cur_name .. "': ")
  if #new_name > 0 and new_name ~= cur_name then
    local params = vim.lsp.util.make_position_params()
    params.newName = new_name
    vim.lsp.buf_request(0, "textDocument/rename", params)
  end
end

local on_attach = function(_, bufnr)
  local telescope_builtin = require("telescope.builtin")

  ks("n", "<leader>rn", rename, { buffer = bufnr, desc = "LSP: Rename" })
  ks("n", "<leader>ca", lsp.buf.code_action, { buffer = bufnr, desc = "LSP: Code action" })

  ks("n", "gd", lsp.buf.definition, { buffer = bufnr, desc = "LSP: Goto definition" })
  ks("n", "gD", lsp.buf.declaration, { buffer = bufnr, desc = "LSP: Goto declaration" })
  ks("n", "gi", lsp.buf.implementation, { buffer = bufnr, desc = "LSP: Goto implementation" })
  ks("n", "gt", lsp.buf.type_definition, { buffer = bufnr, desc = "LSP: Goto type Definition" })

  ks("n", "gr", telescope_builtin.lsp_references, { buffer = bufnr, desc = "LSP: Goto references" })
  ks("n", "gs", telescope_builtin.lsp_dynamic_workspace_symbols,
    { buffer = bufnr, desc = "LSP: Goto workspace symbols" })

  ks("n", "K", lsp.buf.hover, { buffer = bufnr, desc = "LSP: Hover documentation" })
  ks("i", "<C-K>", lsp.buf.signature_help, { buffer = bufnr, desc = "LSP: Signature Documentation" })

  ks("n", "<leader>wa", lsp.buf.add_workspace_folder,
    { buffer = bufnr, desc = "LSP: Add workspace folder" })
  ks("n", "<leader>wr", lsp.buf.remove_workspace_folder,
    { buffer = bufnr, desc = "LSP: Remove workspace folder" })

  ks("n", "<leader>wl", function()
    print(vim.inspect(lsp.buf.list_workspace_folders()))
  end, { buffer = bufnr, desc = "List workspace folders" })

  ks("n", "<leader>mt", lsp.buf.format, { buffer = bufnr, desc = "LSP: Format current buffer" })
end

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
  vim.lsp.handlers.hover, {
    border = "rounded",
  }
)
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(
  vim.lsp.handlers.signature_help, {
    border = "rounded",
  }
)

-- List of configured language servers
local servers = {
  bashls = {},
  clangd = {},
  cssls = {},
  gopls = {},
  jdtls = {
    cmd = {
      "java",
      "-Declipse.application=org.eclipse.jdt.ls.core.id1",
      "-Dosgi.bundles.defaultStartLevel=4",
      "-Declipse.product=org.eclipse.jdt.ls.core.product",
      "-Xms1g",
      "--add-modules=ALL-SYSTEM",
      "--add-opens", "java.base/java.util=ALL-UNNAMED",
      "--add-opens", "java.base/java.lang=ALL-UNNAMED",
      "-jar", "~/.local/share/nvim/mason/share/jdtls/org.eclipse.equinox.launcher.jar",
      "-configuration", "~/.local/share/nvim/mason/share/jdtls/config/config.ini",
      "-data", "~/.local/share/nvim/java"
    },
  },
  lua_ls = {
    Lua = {
      diagnostics = {
        globals = { "vim" }
      },
    },
  },
  omnisharp = {},
  pyright = {},
  tailwindcss = {},
  tsserver = {},
}

-- Nice UI for diagnostics
require("trouble").setup({
  action_keys = {
    cancel = {},
  },
})
ks("n", "<leader>t", require("trouble").open, { desc = "Open trouble split" })

-- Setup neovim lua configuration
require("neodev").setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

-- Ensure the servers above are installed
require("mason-lspconfig").setup({
  ensure_installed = vim.tbl_keys(servers),
})

require("mason-lspconfig").setup_handlers({
  function(server_name)
    require("lspconfig")[server_name].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
    })
  end,
})

require("lspconfig.ui.windows").default_options.border = "rounded"
