-- LSP Configuration & Plugins
return {
  "neovim/nvim-lspconfig",
  dependencies = {
    -- Automatically install LSPs to stdpath
    { "williamboman/mason.nvim", opts = { ui = { border = "rounded" } } },

    -- Useful status updates for LSP
    { "j-hui/fidget.nvim", tag = "legacy", opts = {} },

    -- Better lua LSP functionality
    { "folke/lazydev.nvim", opts = {} },

    -- Adds LSP completion capabilities
    "hrsh7th/cmp-nvim-lsp",

    -- Better java support
    "mfussenegger/nvim-jdtls",
  },
  config = function()
    local lspconfig = require("lspconfig")
    local lspconfig_windows = require("lspconfig.ui.windows")
    local mason_registry = require("mason-registry")
    local cmp_nvim_lsp = require("cmp_nvim_lsp")

    -- Change :LspInfo border
    lspconfig_windows.default_options.border = "rounded"

    ---@class ToolConfig
    ---@field lspconfig_name string
    ---@field mason_name string
    ---@field settings table?

    ---@type ToolConfig[]
    local tools = {
      -- Language servers
      { lspconfig_name = "bashls", mason_name = "bash-language-server" },
      { lspconfig_name = "clangd", mason_name = "clangd" },
      { lspconfig_name = "cssls", mason_name = "css-lsp" },
      { lspconfig_name = "gopls", mason_name = "gopls" },
      { lspconfig_name = "jdtls", mason_name = "jdtls" },
      { lspconfig_name = "rust_analyzer", mason_name = "rust-analyzer" },
      { lspconfig_name = "lua_ls", mason_name = "lua-language-server" },
      { lspconfig_name = "pyright", mason_name = "pyright" },
      { lspconfig_name = "ts_ls", mason_name = "typescript-language-server" },
      { lspconfig_name = "neocmake", mason_name = "neocmakelsp" },

      -- Formatters
      { lspconfig_name = "stylua", mason_name = "stylua" },
      { lspconfig_name = "prettierd", mason_name = "prettierd" },
    }

    -- Install all mason_packages
    local capabilities = cmp_nvim_lsp.default_capabilities()
    for _, tool in ipairs(tools) do
      local package = mason_registry.get_package(tool.mason_name)
      if not package:is_installed() then
        package:install()
      end

      -- Only setup lspconfig on tools it has configs for
      local ok, _ = pcall(require, "lspconfig.configs." .. tool.lspconfig_name)
      if ok then
        lspconfig[tool.lspconfig_name].setup({
          capabilities = capabilities,
          settings = tool.settings,
        })
      end
    end
  end,
}
