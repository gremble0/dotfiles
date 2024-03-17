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

---@param tab Tab
---@return string
local component_name = function(tab)
  local tabpage_is_active = tab.tabnr == vim.fn.tabpagenr()
  local buf_name = vim.fn.bufname(tabpage_get_active_buf(tab.tabnr))
  local component_name = buf_name == "" and "[No Name]" or vim.fn.fnamemodify(buf_name, ":t") -- TODO: get [No Name] from vim api? i think there is some option to change this

  return (tabpage_is_active and HIGHLIGHT_SEL or HIGHLIGHT_NORM) .. component_name
end

---@param tab Tab
---@return string
local component_modified = function(tab)
  return vim.api.nvim_buf_get_option(tabpage_get_active_buf(tab.tabnr), "modified") and "[+]" or ""
end

---@param tab Tab
---@return string
local tabline_make_entry = function(tab)
  return component_separator(tab)
    .. " "
    .. component_icon(tab)
    .. " "
    .. component_name(tab)
    .. " "
    .. component_modified(tab)
    .. "  "
end

vim.o.tabline = "%!v:lua.require('core.tabline')()"

return function()
  local tabline_builder = ""
  -- local debugs = ""
  for _, tab in ipairs(vim.fn.gettabinfo()) do
    -- debugs = debugs .. tabpage .. " " .. vim.api.nvim_buf_get_name(tabpage_get_active_buf(tabpage)) .. ", "
    tabline_builder = tabline_builder .. tabline_make_entry(tab)
  end
  -- print(debugs)

  -- print(tabline_builder)
  return tabline_builder .. "%#TablineFill#"
end
