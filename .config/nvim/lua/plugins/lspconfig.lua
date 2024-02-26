-- LSP Configuration & Plugins
return {
  "neovim/nvim-lspconfig",

  dependencies = {
    -- Automatically install LSPs to stdpath for neovim
    {
      "williamboman/mason.nvim",
      dependencies = "williamboman/mason-lspconfig.nvim",
      config = true,
      opts = { ui = { border = "rounded" } },
    },

    -- Useful status updates for LSP
    { "j-hui/fidget.nvim", tag = "legacy", opts = {} },

    -- Better lua LSP functionality
    { "folke/neodev.nvim", opts = {} },

    -- Better java LSP functionality
    "mfussenegger/nvim-jdtls",
  },

  config = function()
    local cmp_nvim_lsp = require("cmp_nvim_lsp")
    local lspconfig = require("lspconfig")
    local lspconfig_windows = require("lspconfig.ui.windows")
    local mason = require("mason")
    local mason_lspconfig = require("mason-lspconfig")
    local mason_registry = require("mason-registry")

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

    -- Ensure the servers above are installed
    mason.setup()
    mason_lspconfig.setup({
      handlers = {
        function(server)
          lspconfig[server].setup({
            capabilities = vim.tbl_deep_extend(
              "force",
              lsp.protocol.make_client_capabilities(),
              cmp_nvim_lsp.default_capabilities()
            ),
            on_attach = on_attach,
          })
        end,
      },
    })

    -- List of configured language servers
    local mason_packages = {
      "bash-language-server",
      "clangd",
      "css-lsp",
      "gopls",
      "jdtls",
      "lua-language-server",
      "pyright",
      "typescript-language-server",
      "stylua",
      "prettierd",
    }

    -- Install all mason_packages
    for _, tool in ipairs(mason_packages) do
      local package = mason_registry.get_package(tool)
      if not package:is_installed() then
        package:install()
      end
    end
  end,
}
