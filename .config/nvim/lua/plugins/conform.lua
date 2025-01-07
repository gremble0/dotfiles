-- Automatic formatting
local autoformat_cmd = nil

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
  keys = {
    {
      "<leader>mt",
      function()
        require("conform").format({ timeout_ms = 1000, lsp_format = "fallback" })
      end,
      desc = "Format current buffer",
    },
    {
      "<leader>me",
      function()
        autoformat_cmd = autoformat_cmd
          or vim.api.nvim_create_autocmd("BufWritePre", {
            group = vim.api.nvim_create_augroup("ConformAutoFormat", { clear = false }),
            pattern = "*",
            callback = function()
              require("conform").format({ timeout_ms = 1000, lsp_format = "fallback" })
            end,
          })
      end,
      desc = "Enable autoformatting",
    },
    {
      "<leader>md",
      function()
        if autoformat_cmd then
          vim.api.nvim_del_autocmd(autoformat_cmd)
          autoformat_cmd = nil
        end
      end,
      desc = "Disable autoformatting",
      silent = true,
    },
  },
  cmd = { "ConformInfo" },
}
