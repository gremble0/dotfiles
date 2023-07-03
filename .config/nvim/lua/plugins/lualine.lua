-- [[ Configure Lualine ]]
require("lualine").setup {
    options = {
      component_separators = "",
      section_separators = "",
    },
    sections = {
      lualine_a = { "mode" },
      lualine_b = { "filename" },
      lualine_c = { "branch", "diff" },
      lualine_x = { "diagnostics" },
      lualine_y = { "location" },
      lualine_z = {},
    }
}
