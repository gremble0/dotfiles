-- TODO: keep signaturehelp while in insert mode
local cmp_nvim_lsp = require("cmp_nvim_lsp")
local lsp_config = require("lspconfig")
local lsp_config_windows = require("lspconfig.ui.windows")
local mason = require("mason")
local mason_lspconfig = require("mason-lspconfig")
local mason_registry = require("mason-registry")
local neodev = require("neodev")

local lsp = vim.lsp
local ks = vim.keymap.set

lsp_config_windows.default_options.border = "rounded"
lsp.handlers["textDocument/hover"] = lsp.with(lsp.handlers.hover, { border = "rounded" })
lsp.handlers["textDocument/signatureHelp"] = lsp.with(lsp.handlers.signature_help, { border = "rounded" })

local on_attach = function(_, bufnr)
  local telescope_builtin = require("telescope.builtin")

  ks("n", "<leader>rn", lsp.buf.rename, { buffer = bufnr, desc = "LSP: Rename" })
  ks("n", "<leader>ca", lsp.buf.code_action, { buffer = bufnr, desc = "LSP: Code action" })

  ks("n", "gd", lsp.buf.definition, { buffer = bufnr, desc = "LSP: Goto definition" })
  ks("n", "gD", lsp.buf.declaration, { buffer = bufnr, desc = "LSP: Goto declaration" })
  ks("n", "gi", lsp.buf.implementation, { buffer = bufnr, desc = "LSP: Goto implementation" })
  ks("n", "gt", lsp.buf.type_definition, { buffer = bufnr, desc = "LSP: Goto type Definition" })

  ks("n", "gr", telescope_builtin.lsp_references, { buffer = bufnr, desc = "LSP: Goto references" })

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
  lua_ls = { Lua = { diagnostics = { globals = { "vim" } } } },
  pyright = {},
  tsserver = {},
}

-- Setup neovim lua configuration
neodev.setup()

-- Ensure the servers above are installed
mason.setup()
mason_lspconfig.setup({
  ensure_installed = vim.tbl_keys(servers),
  handlers = {
    function(server_name)
      lsp_config[server_name].setup({
        capabilities = cmp_nvim_lsp.default_capabilities(lsp.protocol.make_client_capabilities()),
        on_attach = on_attach,
        settings = servers[server_name],
      })
    end,
  },
})

-- Non lsp mason things cannot be added to ensure_installed
local tools = { "stylua", "prettierd", "clang-format" }

for _, tool in ipairs(tools) do
  local package = mason_registry.get_package(tool)
  if not package:is_installed() then
    package:install()
  end
end
