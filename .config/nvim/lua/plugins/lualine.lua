-- [[ Configure Lualine ]]
-- Use gitsigns to get info about file diffs (default diff only loads on save and doesnt always load properly)

local palette = require("yellowbeans.styles").palette

local function diff_source()
  local gitsigns = vim.b.gitsigns_status_dict
  if gitsigns then
    return {
      added = gitsigns.added,
      modified = gitsigns.changed,
      removed = gitsigns.removed
    }
  end
end

local lsp_section = function()
  local buf_ft = vim.api.nvim_buf_get_option(0, 'filetype')
  local clients = vim.lsp.get_active_clients()
  if next(clients) == nil then
    return ""
  end
  for _, client in ipairs(clients) do
    local filetypes = client.config.filetypes
    if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
      return "  " .. client.name
    end
  end
  return ""
end

require("lualine").setup {
  options = {
    component_separators = "",
    section_separators = "",
    refresh = { statusline = 200 },
  },
  sections = {
    lualine_a = {
      {
        "mode",
        icons_enabled = true,
      }
    },
    lualine_b = {
      {
        "filetype",
        icon_only = true,
        colored = false,
        color = { fg = palette.fg, bg = palette.black_three },
      },
      {
        "filename",
        color = { fg = palette.fg, bg = palette.black_three, gui = "bold" },
      },
    },
    lualine_c = {
      {
        "branch",
        color = { fg = palette.white_one, bg = "#202020" },
      },
      {
        "diff",
        sources = diff_source,
        color = { fg = palette.white_one, bg = "#202020" },
      }
    },
    lualine_x = {
      {
        "diagnostics",
        color = { fg = palette.white_one, bg = "#202020" },
      },
      {
        lsp_section,
        color = { fg = palette.white_one, bg = "#202020" },
      },
    },
    lualine_y = {
      {
        "location",
        color = { fg = palette.fg, bg = palette.black_three, gui = "bold" },
      },
    },
    lualine_z = {},
  }
}
