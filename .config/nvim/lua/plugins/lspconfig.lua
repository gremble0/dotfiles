-- LSP Configuration & Plugins
return {
  "neovim/nvim-lspconfig",

  dependencies = {
    -- Automatically install LSPs to stdpath for neovim
    { "williamboman/mason.nvim", opts = { ui = { border = "rounded" } } },

    -- Useful status updates for LSP
    { "j-hui/fidget.nvim", tag = "legacy", opts = {} },

    -- Better lua LSP functionality
    { "folke/neodev.nvim", opts = {} },

    -- Better java LSP functionality
    "mfussenegger/nvim-jdtls",
  },

  config = function()
    local lspconfig = require("lspconfig")
    local lspconfig_windows = require("lspconfig.ui.windows")
    local mason = require("mason")
    local mason_registry = require("mason-registry")

    lspconfig_windows.default_options.border = "rounded"

    -- Ensure the servers above are installed
    mason.setup()

    -- List of configured language servers and other tools
    local tools = {
      ["bash-language-server"] = "bashls",
      ["clangd"] = "clangd",
      ["css-lsp"] = "cssls",
      ["gopls"] = "gopls",
      ["jdtls"] = "jdtls",
      ["lua-language-server"] = "lua_ls",
      ["pyright"] = "pyright",
      ["typescript-language-server"] = "tsserver",
      ["stylua"] = "stylua",
      ["prettierd"] = "prettierd",
    }

    -- Install all mason_packages
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    for tool_mason_name, tool_lspconfig_name in pairs(tools) do
      local package = mason_registry.get_package(tool_mason_name)
      if not package:is_installed() then
        package:install()
      end

      -- Non LSP tools cannot be setup by lspconfig
      if vim.tbl_contains(package.spec.categories, "LSP") then
        lspconfig[tool_lspconfig_name].setup({
          capabilities = capabilities,
        })
      end
    end
  end,
}
