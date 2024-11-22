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

  -- Only setting BufWritePost as event will not also format the buffer first time the
  -- autocommand is executed. This initializes the autocommand for formatting before the
  -- plugin is loaded which means it will also format on the first BufWritePost event
  event = function(_, _)
    vim.api.nvim_create_autocmd("BufWritePost", {
      group = vim.api.nvim_create_augroup("Conform", {}),
      callback = function()
        require("conform").format({ timeout_ms = 1000, lsp_format = "fallback" })
      end,
    })

    return { "BufWritePost" }
  end,

  keys = {
    { "<leader>mt", ":lua require('conform').format()<CR>", desc = "Format current buffer", silent = true },
  },

  cmd = { "ConformInfo", "ConformEnable" },
}
