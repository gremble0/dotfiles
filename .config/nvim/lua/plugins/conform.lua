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
      callback = function(args)
        if not vim.g.disable_autoformat then
          require("conform").format({ bufnr = args.buf, timeout_ms = 1000, lsp_format = "fallback" })
        end
      end,
    })

    vim.api.nvim_create_user_command("ConformDisable", function()
      vim.g.disable_autoformat = true
    end, {
      desc = "Disable autoformat-on-save",
    })

    vim.api.nvim_create_user_command("ConformEnable", function()
      vim.g.disable_autoformat = false
    end, {
      desc = "Re-enable autoformat-on-save",
    })

    require("conform").setup(opts)
  end,
  keys = {
    { "<leader>mt", ":lua require('conform').format()<CR>", desc = "Format current buffer", silent = true },
  },
  cmd = { "ConformInfo", "ConformEnable", "ConformDisable" },
}
