-- Automatic formatting
return {
  "stevearc/conform.nvim",

  opts = {
    format_on_save = {
      timeout_ms = 500,
      lsp_fallback = true,
    },
    formatters_by_ft = {
      lua = { "stylua" },
      javascript = { "prettierd" },
    },
  },

  -- Load plugin when writing to file/<leader>mt key is pressed/:ConformInfo command is executed
  event = "BufWritePost",

  keys = {
    { "<leader>mt", ":lua require('conform').format()<CR>", desc = "Format current buffer", silent = true },
  },

  cmd = "ConformInfo",
}
