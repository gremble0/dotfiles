-- LSP Configuration & Plugins
---@type PluginSpec
return {
  spec = { src = "https://github.com/neovim/nvim-lspconfig" },
  dependencies = {
    -- Automatically install LSPs to stdpath
    { src = "https://github.com/williamboman/mason.nvim" },

    -- Useful status updates for LSP
    { src = "https://github.com/j-hui/fidget.nvim" },

    -- Better lua LSP functionality
    { src = "https://github.com/folke/lazydev.nvim" },

    -- Adds LSP completion capabilities
    { src = "https://github.com/hrsh7th/cmp-nvim-lsp" },

    -- Better java support
    { src = "https://github.com/mfussenegger/nvim-jdtls" },
  },
  setup = {
    setup = function()
      local lspconfig = require("lspconfig")
      local mason_registry = require("mason-registry")
      local cmp_nvim_lsp = require("cmp_nvim_lsp")

      require("mason").setup({
        ui = {
          backdrop = 100,
        },
      })
      require("fidget").setup()
      require("lazydev").setup()

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
  },
}
