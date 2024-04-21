-- Statusline
return {
  "nvim-lualine/lualine.nvim",

  dependencies = "nvim-tree/nvim-web-devicons",

  config = function()
    -- TODO: use opts?
    local lualine = require("lualine")

    -- Make section for lsp info in statusline
    local lsp_section = function()
      local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
      local clients = vim.lsp.get_active_clients()

      for _, client in ipairs(clients) do
        local filetypes = client.config.filetypes
        if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
          return " " .. client.name
        end
      end

      return ""
    end

    local function diff_source()
      ---@diagnostic disable-next-line
      local gitsigns = vim.b.gitsigns_status_dict
      if gitsigns then
        return {
          added = gitsigns.added,
          modified = gitsigns.changed,
          removed = gitsigns.removed,
        }
      end
    end

    lualine.setup({
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
