-- LSP Configuration & Plugins
return {
  "neovim/nvim-lspconfig",

  dependencies = {
    -- Automatically install LSPs to stdpath
    { "williamboman/mason.nvim", opts = { ui = { border = "rounded" } } },

    -- Useful status updates for LSP
    { "j-hui/fidget.nvim", tag = "legacy", opts = {} },

    -- Better lua LSP functionality
    { "folke/neodev.nvim", opts = {} },

    -- Adds LSP completion capabilities
    "hrsh7th/cmp-nvim-lsp",
  },

  config = function()
    local lspconfig = require("lspconfig")
    local lspconfig_windows = require("lspconfig.ui.windows")
    local mason_registry = require("mason-registry")
    local cmp_nvim_lsp = require("cmp_nvim_lsp")

    lspconfig_windows.default_options.border = "rounded"

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
    local capabilities =
      vim.tbl_deep_extend("force", vim.lsp.protocol.make_client_capabilities(), cmp_nvim_lsp.default_capabilities())
    for tool_mason_name, tool_lspconfig_name in pairs(tools) do
      local package = mason_registry.get_package(tool_mason_name)
      if not package:is_installed() then
        package:install()
      end

      -- Only LSP tools can be setup by lspconfig
      if vim.tbl_contains(package.spec.categories, "LSP") then
        lspconfig[tool_lspconfig_name].setup({
          capabilities = capabilities,
        })
      end
    end
  end,
}
