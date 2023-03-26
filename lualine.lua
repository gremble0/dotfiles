local custom_codedark = require"lualine.themes.codedark"
custom_codedark.normal.a.bg = "#cdab89"
custom_codedark.normal.b.fg = "#ffffff"

custom_codedark.insert.a.bg = "#9e96b6"
custom_codedark.insert.b.fg = "#ffffff"

custom_codedark.visual.a.bg = "#d49191"
custom_codedark.visual.b.fg = "#ffffff"

custom_codedark.replace.a.bg = "#dbcaa9"
custom_codedark.replace.b.fg = "#ffffff"

--custom_codedark.command.a.bg = "#cdab89" d49191


require('lualine').setup {
    options = {
        theme = custom_codedark,
        component_separators = { left = '', right = ''},
        section_separators = { left = '', right = ''},
        icons_enabled = false,
    },
    sections = {
        lualine_a = {'mode'}, 
        lualine_b = {'branch', 'diff', 'diagnostics'},
        lualine_c = {'filename'},
        lualine_x = {'filetype'},
        lualine_y = {'progress'},
        lualine_z = {'location'}
    }
}
