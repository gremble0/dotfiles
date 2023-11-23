-- [[ Configure Lualine ]]

-- Make section for lsp info for in statusline
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
    theme = require("yellowbeans.lualine")
  },
  sections = {
    lualine_a = { { "mode", icons_enabled = true } },
    lualine_b = {
      {
        "filetype",
        icon_only = true,
        colored = false,
      },
      { "filename", path = 3 },
    },
    lualine_c = { "branch", "diff" },
    lualine_x = { "diagnostics", lsp_section },
    lualine_y = { "location" },
    lualine_z = { "searchcount" },
  },
}
