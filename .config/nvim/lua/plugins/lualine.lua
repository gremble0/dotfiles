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

    lualine.setup({
      options = {
        component_separators = "",
        refresh = { statusline = 200 },
        theme = require("yellowbeans.lualine"),
      },
      sections = {
        lualine_a = { { "mode", icons_enabled = true } },
        lualine_b = { "filename" },
        lualine_c = { "branch", "diff" },
        lualine_x = { "diagnostics", lsp_section },
        lualine_y = { { "filetype" } },
        lualine_z = { "location" },
      },
    })
  end,
}
