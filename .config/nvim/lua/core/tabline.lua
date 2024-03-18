-- Define some shortcuts for highlight groups
local HIGHLIGHT_NORM = "%#TablineNorm#"
local HIGHLIGHT_NORM_SEP = "%#TablineNormSep#"
local HIGHLIGHT_SEL = "%#TablineSel#"
local HIGHLIGHT_SEL_SEP = "%#TablineSelSep#"

---@param tabpage integer
---@return integer id of active buffer in the given tabpage
local tabpage_get_active_buf = function(tabpage)
  local buflist = vim.fn.tabpagebuflist(tabpage)
  local winnr = vim.fn.tabpagewinnr(tabpage)

  return type(buflist) == "number" and buflist or buflist[winnr]
end

-- ---@param entry string
-- ---@param width integer
-- ---@return string
-- local entry_pad_to_width = function(entry, width)
--   local entry_len = entry:len()
--   return string.rep(" ", entry_len - math.floor(width / 2))
--     .. entry
--     .. string.rep(" ", entry_len - math.ceil(width / 2))
-- end
--
-- ---Either pads or trims the entry string to the given width
-- ---@param entry string
-- ---@param width integer
-- ---@return string
-- local entry_fit_to_width = function(entry, width)
--   return entry_rendered_length(entry) > width and name_trim_to_width(entry, width) or entry_pad_to_width(entry, width)
-- end

---This class is a mockup of the returntype of vim.fn.gettabinfo()
---@class Tab
---@field tabnr integer
---@field variables table
---@field windows integer[]

---@param tab Tab
---@return string
local component_separator = function(tab)
  return tab.tabnr == vim.fn.tabpagenr() and (HIGHLIGHT_SEL_SEP .. "▎") or (HIGHLIGHT_NORM_SEP .. "▏")
end

---@param tab Tab
---@return string
local component_icon = function(tab)
  local tabpage_active_buf = tabpage_get_active_buf(tab.tabnr) -- TODO: fix?
  local buf_name = vim.fn.bufname(tabpage_active_buf)
  local buf_ft = vim.api.nvim_buf_get_option(tabpage_active_buf, "ft")
  -- TODO: pcall() ?
  local icon, icon_hl = require("nvim-web-devicons").get_icon(buf_name, buf_ft, { default = true })

  return "%#" .. icon_hl .. "#" .. icon
end

---@param name string
---@param width integer
---@return string
local name_trim_to_width = function(name, width)
  return name:len() > width and name:sub(1, width - 3) .. "..." or name
end

---@param tab Tab
---@return string
local component_name = function(tab)
  local tabpage_is_active = tab.tabnr == vim.fn.tabpagenr()
  local buf_name = vim.fn.bufname(tabpage_get_active_buf(tab.tabnr))
  local name = buf_name == "" and "[No Name]" or vim.fn.fnamemodify(buf_name, ":t")
  -- TODO: get [No Name] from vim api? i think there is some option to change this
  -- TODO: expand when no name gets set ref: fugitive

  return (tabpage_is_active and HIGHLIGHT_SEL or HIGHLIGHT_NORM) .. name_trim_to_width(name, 22) -- TODO: fix hardcoded 22 and 30
end

---@param tab Tab
---@return string
local component_modified = function(tab)
  return vim.api.nvim_buf_get_option(tabpage_get_active_buf(tab.tabnr), "modified") and "[+]" or ""
end

---@param entry string
---@return integer
local entry_rendered_length = function(entry)
  local len_iter = entry:len()
  for highlight in entry:gmatch("%%#.-#") do -- Matches inline hl groups like %#HighlightGroup#
    len_iter = len_iter - highlight:len()
  end

  return len_iter
end

---@param entry string
---@param width integer
---@return string left_padding, string right_padding
local entry_add_padding = function(entry, width)
  local total_padding = width - entry_rendered_length(entry) -- TODO: entry:rendered_length() ?

  return (" "):rep(math.floor(total_padding / 2)), (" "):rep(math.ceil(total_padding / 2) + 1)
end

---@param tab Tab
---@return string
local tabline_make_entry = function(tab)
  local separator = component_separator(tab)
  local icon = " " .. component_icon(tab)
  local name = " " .. component_name(tab)
  local modified = " " .. component_modified(tab)
  modified = modified:len() > 1 and (" " .. modified) or ""

  local entry_unpadded = separator .. icon .. name .. modified .. "  "
  local left_padding, right_padding = entry_add_padding(entry_unpadded, 30) -- TODO: fix hardcoded 30 and 22

  return separator .. left_padding .. icon .. name .. modified .. right_padding
end

vim.o.tabline = "%!v:lua.require('core.tabline')()"

return function()
  local tabline_builder = ""
  for _, tab in ipairs(vim.fn.gettabinfo()) do
    tabline_builder = tabline_builder .. tabline_make_entry(tab)
  end

  return tabline_builder .. "%#TablineFill#"
end
