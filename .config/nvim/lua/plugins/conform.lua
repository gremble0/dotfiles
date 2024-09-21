-- Automatic formatting
return {
  "stevearc/conform.nvim",

  config = function()
    vim.api.nvim_create_user_command("ConformDisable", function(args)
      if args.bang then
        vim.b.disable_autoformat = true
      else
        vim.g.disable_autoformat = true
      end
    end, {
      desc = "Disable autoformat-on-save",
      bang = true,
    })

    vim.api.nvim_create_user_command("ConformEnable", function()
      vim.b.disable_autoformat = false
      vim.g.disable_autoformat = false
    end, {
      desc = "Re-enable autoformat-on-save",
    })

    require("conform").setup({
      format_on_save = function(bufnr)
        if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
          return
        end
        return { timeout_ms = 500, lsp_format = "fallback" }
      end,

      formatters_by_ft = {
        lua = { "stylua" },
        javascript = { "prettierd" },
      },
    })
  end,

  -- Load plugin when writing to file/<leader>mt key is pressed/:ConformInfo command is executed
  event = "BufWritePost",

  keys = {
    { "<leader>mt", ":lua require('conform').format()<CR>", desc = "Format current buffer", silent = true },
  },

  cmd = "ConformInfo",
}
