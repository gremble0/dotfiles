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
    local autoformat_cmd = nil

    vim.api.nvim_create_user_command("ConformDisable", function()
      if autoformat_cmd then
        vim.api.nvim_del_autocmd(autoformat_cmd)
        autoformat_cmd = nil
      end
    end, {
      desc = "Disable autoformat-on-save",
    })

    vim.api.nvim_create_user_command("ConformEnable", function()
      autoformat_cmd = vim.api.nvim_create_autocmd("BufWritePre", {
        group = vim.api.nvim_create_augroup("ConformAutoFormat", { clear = false }),
        pattern = "*",
        callback = function()
          require("conform").format({ timeout_ms = 1000, lsp_format = "fallback" })
        end,
      })
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
