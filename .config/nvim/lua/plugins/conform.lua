-- Automatic formatting
local make_autoformat_autocmd = function()
  return vim.api.nvim_create_autocmd("BufWritePre", {
    group = vim.api.nvim_create_augroup("ConformAutoFormat", { clear = false }),
    pattern = "*",
    callback = function()
      require("conform").format({ timeout_ms = 1000, lsp_format = "fallback" })
    end,
  })
end

--- Enable automatic formatting by default - set to nil to disable by default
---@type integer?
local autoformat_cmd = make_autoformat_autocmd()

---@type PluginSpec
return {
  vim_pack_spec = { src = "https://github.com/stevearc/conform.nvim" },
  setup = function()
    require("conform").setup({
      formatters_by_ft = {
        lua = { "stylua" },
        javascript = { "prettierd" },
        javascriptreact = { "prettierd" },
        typescript = { "prettierd" },
        typescriptreact = { "prettierd" },
      },
    })

    vim.keymap.set("n", "<leader>mt", function()
      require("conform").format({ timeout_ms = 1000, lsp_format = "fallback" })
    end, { desc = "Format current buffer" })

    vim.keymap.set("n", "<leader>me", function()
      autoformat_cmd = autoformat_cmd or make_autoformat_autocmd()
    end, { desc = "Enable autoformatting" })

    vim.keymap.set("n", "<leader>md", function()
      if autoformat_cmd then
        vim.api.nvim_del_autocmd(autoformat_cmd)
        autoformat_cmd = nil
      end
    end, { desc = "Disable autoformatting", silent = true })
  end,
}
