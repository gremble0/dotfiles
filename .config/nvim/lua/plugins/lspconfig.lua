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

    local tools = {
      -- Language servers
      bashls = { mason_name = "bash-language-server" },
      clangd = { mason_name = "clangd" },
      cssls = { mason_name = "css-lsp" },
      gopls = { mason_name = "gopls" },
      jdtls = { mason_name = "jdtls" },
      rust_analyzer = { mason_name = "rust-analyzer" },
      lua_ls = { mason_name = "lua-language-server" },
      pyright = { mason_name = "pyright" },
      ts_ls = { mason_name = "typescript-language-server" },

      -- Formatters
      stylua = { mason_name = "stylua" },
      prettierd = { mason_name = "prettierd" },
      angularls = { mason_name = "angular-language-server" },
    }

    -- Install all mason_packages
    local capabilities = cmp_nvim_lsp.default_capabilities()
    for tool_lspconfig_name, tool_config in pairs(tools) do
      local package = mason_registry.get_package(tool_config.mason_name)
      if not package:is_installed() then
        package:install()
      end

      -- Only LSP tools can be setup by lspconfig
      if vim.tbl_contains(package.spec.categories, "LSP") then
        lspconfig[tool_lspconfig_name].setup({
          capabilities = capabilities,
          settings = tool_config.settings,
        })
      end
    end
  end,
}
