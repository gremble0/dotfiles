-- [[ Configure Lualine ]]
-- Use gitsigns to get info about file diffs (default diff only loads on save and doesnt always load properly)
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

require("lualine").setup {
    options = {
      component_separators = "",
      section_separators = "",
      refresh = { statusline = 200 },
    },
    sections = {
      lualine_a = { "mode" },
      lualine_b = { "filename" },
      lualine_c = { "branch", { "diff", source = diff_source } },
      lualine_x = { "diagnostics" },
      lualine_y = { "location" },
      lualine_z = {},
    }
}
