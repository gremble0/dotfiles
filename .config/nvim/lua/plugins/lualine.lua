-- Statusline
return {
  "nvim-lualine/lualine.nvim",
  dependencies = "nvim-tree/nvim-web-devicons",
  config = function()
    -- Make section for lsp info in statusline
    local lsp_section = function()
      local buf_ft = vim.api.nvim_get_option_value("filetype", { buf = 0 })
      local clients = vim.lsp.get_clients()

      for _, client in ipairs(clients) do
        local filetypes = client.config.filetypes
        if filetypes and vim.list_contains(filetypes, buf_ft) then
          return " " .. client.name
        end
      end

      return ""
    end

    local diff_source = function()
      local ok, gitsigns = pcall(vim.api.nvim_buf_get_var, 0, "gitsigns_status_dict")
      if ok then
        return {
          added = gitsigns.added,
          modified = gitsigns.changed,
          removed = gitsigns.removed,
        }
      end
    end
    require("lualine").setup({
      options = {
        component_separators = "",
        refresh = { statusline = 200 },
        theme = require("lualine.themes.yellowbeans-monochrome"),
      },
      sections = {
        lualine_a = { "mode" },
        lualine_b = { { "filename", symbols = { modified = "●", readonly = "" } } },
        lualine_c = { "branch", { "diff", source = diff_source } },
        lualine_x = { "diagnostics", lsp_section },
        lualine_y = { "filetype" },
        lualine_z = { "location" },
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = { "filename" },
        lualine_c = { "branch", { "diff", source = diff_source } },
        lualine_x = { "diagnostics", lsp_section },
        lualine_y = { "filetype" },
        lualine_z = { "location" },
      },
    })
  end,
}
