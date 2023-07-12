local hi = function(group, colors)
  vim.api.nvim_set_hl(0, group, colors)
end

local bg = "#151515"
local fg = "#cccccc"
local black_one = "#191919"
local black_two = "#1c1c1c"
local white_one = "#aaaaaa"
local white_two = "#c6b6ee"
local gray_one = "#333333"

local gold_yellow = "#e1b655"
local koromiko_yellow = "#ffb964"
local olive_green = "#9aae6b"
local hunter_green = "#355e3b"
local sienna_orange = "#cf6a4c"
local morningglory_blue = "#8fbfdc"
local shipcove_blue = "#8197bf"
local perano_blue = "#b0d0f0"
local hoki_blue = "#668799"
local wewak_pink = "#f0a0c0"

local bad = "#d22b2b"
local neutral = "#ffa500"
local good = "#60a840"


-- Main highlight groups
hi("Normal", { fg = fg, bg = bg })
hi("Cursor", { fg = bg, bg = fg })
-- hi("EndOfBuffer", { fg = "#ffffff", bg = "#151515" })

-- Background things
hi("SignColumn", { fg = fg, bg = bg })

hi("ColorColumn", { bg = black_one })
hi("CursorColumn", { link = "ColorColumn" })
hi("CursorLine", { link = "ColorColumn" })
hi("VertSplit", { fg = black_two })

hi("StatusLine", { fg = fg, bg = black_one })
hi("StatusLineNC", { fg = white_one, bg = black_one })

hi("Visual", { bg = gray_one })

-- Text
hi("Comment", { fg = "#606060" })
hi("LineNr", { link = "Comment" })
hi("CursorLineNr", { link = "Normal" })
hi("MatchParen", { fg = good })
hi("Title", { fg = good })

hi("Bold", { bold = true })
hi("Italic", { italic = true })
hi("Underlined", { underline = true })

hi("Todo", { fg = hunter_green })

-- Menus
hi("Pmenu", { fg = fg, bg = black_two })
hi("PmenuSel", { fg = bg, bg = gold_yellow })
hi("Search", { fg = bg, bg = gold_yellow })

-- Diff mode highlight groups (used as links as well)
hi("DiffAdd", { fg = good })
hi("DiffChange", { fg = neutral })
hi("DiffDelete", { fg = bad }) -- maybe change in alacritty too
hi("DiffText", { italic = true })

-- Bad things
hi("Error", { fg = bad })
hi("ErrorMsg", { link = "Error" })

-- Other
hi("Directory", { fg = gold_yellow })

-- Preferred groups for syntax highlighting (other groups also refer to these)
hi("Constant", { fg = sienna_orange })
hi("Character", { link = "Constant" })
hi("Number", { link = "Constant" })
hi("Boolean", { link = "Constant" })
hi("Float", { link = "Constant" })

hi("String", { fg = olive_green })
hi("StringDelimiter", { link = "String" })

hi("Identifier", { fg = white_two })

hi("Function", { fg = gold_yellow })

hi("Statement", { fg = perano_blue })
hi("Conditional", { link = "Statement" })
hi("Repeat", { link = "Statement" })
hi("Label", { link = "Statement" })

hi("Operator", { fg = morningglory_blue })

hi("PreProc", { fg = gold_yellow })
hi("Include", { link = "PreProc" })
hi("Macro", { link = "PreProc" })
hi("PreCondit", { link = "PreProc" })

hi("Type", { fg = koromiko_yellow })
hi("StorageClass", { link = "Type" })
hi("Typedef", { link = "Type" })

hi("Structure", { fg = morningglory_blue })

hi("Delimiter", { fg = hoki_blue })

-- Plugin settings
-- GitSigns
hi("GitSignsAdd", { fg = good })
hi("GitSignsChange", { fg = neutral })
hi("GitSignsDelete", { fg = bad })

-- Treesitter
hi("TSNamespace", { fg = wewak_pink })
hi("TSVariable", { link = "Normal" })
hi("TSDelimiter", { fg = hoki_blue })
hi("TSEmphasis", { italic = true })
hi("TSUnderline", { underline = true })
hi("TSStrike", { strikethrough = true })
hi("TSURI", { fg = morningglory_blue, underline = true})

-- Telescope
hi("TelescopeBorder", { fg = black_two })
hi("TelescopePromptTitle", { fg = fg })
hi("TelescopePromptCounter", { link = "Comment" })
hi("TelescopePreviewTitle", { link = "TelescopePromptTitle" })
hi("TelescopeResultsTitle", { link = "TelescopePromptTitle" })
hi("TelescopeSelection", { fg = gold_yellow })

-- Nvim-cmp
-- vim.api.nvim_get_hl_id_by_name
-- ^ Maybe useful function instead of hardcoding each "kind"? only returns id not color code
hi("CmpItemKindClass", { fg = "#8fbedc" })
hi("CmpItemKindInterface", { fg = "#8fbedc" })
hi("CmpItemKindKeyword", { fg = "#b2d1f0" })
hi("CmpItemKindMethod", { fg = "#e1b655" })
hi("CmpItemKindFunction", { fg = "#e1b655" })
hi("CmpItemKindVariable", { fg = "#c5b5ee" })
hi("CmpItemKindModule", { fg = "#c5b5ee" })
hi("CmpItemKindField", { fg = "#c5b5ee" })
