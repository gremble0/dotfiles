-- Automatic formatting
return {
  "stevearc/conform.nvim",
  opts = {
    formatters_by_ft = {
      lua = { "stylua" },
      javascript = { "prettierd" },
      javascriptreact = { "prettierd" },
      typescript = { "prettierd" },
      typescriptreact = { "prettierd" },
    },
  },
  config = function(_, opts)
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = vim.api.nvim_create_augroup("ConformAutoFormat", { clear = false }),
      pattern = "*",
      callback = function()
        if vim.g.enable_autoformat then
          require("conform").format({ timeout_ms = 1000, lsp_format = "fallback" })
        end
      end,
    })

    vim.api.nvim_create_user_command("ConformDisable", function()
      vim.g.enable_autoformat = false
    end, {
      desc = "Disable autoformat-on-save",
    })

    vim.api.nvim_create_user_command("ConformEnable", function()
      vim.g.enable_autoformat = true
    end, {
      desc = "Re-enable autoformat-on-save",
    })

    require("conform").setup(opts)
  end,
  keys = {
    {
      "<leader>mt",
      ":lua require('conform').format({ timeout_ms = 1000, lsp_format = 'fallback' })<CR>",
      desc = "Format current buffer",
      silent = true,
    },
    { "<leader>me", ":ConformEnable<CR>", desc = "Enable autoformatting", silent = true },
    { "<leader>md", ":ConformDisable<CR>", desc = "Disable autoformatting", silent = true },
  },
  cmd = { "ConformInfo", "ConformEnable", "ConformDisable" },
}
