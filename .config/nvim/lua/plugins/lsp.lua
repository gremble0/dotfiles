local cmp_nvim_lsp = require("cmp_nvim_lsp")
local lspconfig = require("lspconfig")
local lspconfig_windows = require("lspconfig.ui.windows")
local mason = require("mason")
local mason_lspconfig = require("mason-lspconfig")
local mason_registry = require("mason-registry")
local neodev = require("neodev")

local lsp = vim.lsp
local ks = vim.keymap.set

lspconfig_windows.default_options.border = "rounded"
lsp.handlers["textDocument/hover"] = lsp.with(lsp.handlers.hover, { border = "rounded" })
lsp.handlers["textDocument/signatureHelp"] = lsp.with(lsp.handlers.signature_help, { border = "rounded" })

local on_attach = function(_, bufnr)
  ks("n", "<leader>rn", lsp.buf.rename, { buffer = bufnr, desc = "LSP: Rename" })
  ks("n", "<leader>ca", lsp.buf.code_action, { buffer = bufnr, desc = "LSP: Code action" })

  ks("n", "gd", lsp.buf.definition, { buffer = bufnr, desc = "LSP: Goto definition" })
  ks("n", "gD", lsp.buf.declaration, { buffer = bufnr, desc = "LSP: Goto declaration" })
  ks("n", "gi", lsp.buf.implementation, { buffer = bufnr, desc = "LSP: Goto implementation" })
  ks("n", "gt", lsp.buf.type_definition, { buffer = bufnr, desc = "LSP: Goto type Definition" })

  ks("n", "gr", lsp.buf.references, { buffer = bufnr, desc = "LSP: Goto references" })

  ks("n", "K", lsp.buf.hover, { buffer = bufnr, desc = "LSP: Hover documentation" })
  ks("i", "<C-S-K>", lsp.buf.signature_help, { buffer = bufnr, desc = "LSP: Signature Documentation" })
end

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
      "--add-opens",
      "java.base/java.util=ALL-UNNAMED",
      "--add-opens",
      "java.base/java.lang=ALL-UNNAMED",
      "-jar",
      "~/.local/share/nvim/mason/share/jdtls/org.eclipse.equinox.launcher.jar",
      "-configuration",
      "~/.local/share/nvim/mason/share/jdtls/config/config.ini",
      "-data",
      "~/.local/share/nvim/java",
    },
  },
  lua_ls = {
    settings = {
      Lua = { diagnostics = { globals = { "vim" } } },
    },
  },
  pyright = {},
  tsserver = {},
}

-- Setup neovim lua configuration
neodev.setup()

local capabilities =
  vim.tbl_deep_extend("force", lsp.protocol.make_client_capabilities(), cmp_nvim_lsp.default_capabilities())

-- Ensure the servers above are installed
mason.setup()
mason_lspconfig.setup({
  ensure_installed = vim.tbl_keys(servers),
  handlers = {
    function(server_name)
      local server = servers[server_name] or {}
      lspconfig[server_name].setup({
        cmd = server.cmd,
        settings = server.settings,
        filetypes = server.filetypes,
        capabilities = vim.tbl_deep_extend("force", {}, capabilities, server.capabilities or {}),
        on_attach = on_attach,
      })
    end,
  },
})

-- Non lsp mason things cannot be added to ensure_installed
local tools = { "stylua", "prettierd" }

for _, tool in ipairs(tools) do
  local package = mason_registry.get_package(tool)
  if not package:is_installed() then
    package:install()
  end
end
