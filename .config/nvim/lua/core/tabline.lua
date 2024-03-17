-- Define some shortcuts for highlight groups
local HIGHLIGHT_NORM = "%#TablineNorm#"
local HIGHLIGHT_NORM_SEP = "%#TablineNormSep#"
local HIGHLIGHT_SEL = "%#TablineSel#"
local HIGHLIGHT_SEL_SEP = "%#TablineSelSep#"

local tabpage_get_active_buf = function(tabpage)
  local buflist = vim.fn.tabpagebuflist(tabpage)
  local winnr = vim.fn.tabpagewinnr(tabpage)

  return type(buflist) == "number" and buflist or buflist[winnr]
end

---@param tabpage integer
---@return string
local component_separator = function(tabpage)
  return tabpage == vim.fn.tabpagenr() and (HIGHLIGHT_SEL_SEP .. "▎") or (HIGHLIGHT_NORM_SEP .. "▏")
end

---@param tabpage integer
---@return string
local component_icon = function(tabpage)
  local tabpage_active_buf = tabpage_get_active_buf(tabpage)
  local buf_name = vim.fn.bufname(tabpage_active_buf)
  local buf_ft = vim.api.nvim_buf_get_option(tabpage_active_buf, "ft")
  -- TODO: pcall() ?
  local icon, icon_hl = require("nvim-web-devicons").get_icon(buf_name, buf_ft, { default = true })

  return "%#" .. icon_hl .. "#" .. icon
end

---@param tabpage integer
---@return string
local component_name = function(tabpage)
  local tabpage_is_active = tabpage == vim.fn.tabpagenr()
  local buf_name = vim.fn.bufname(tabpage_get_active_buf(tabpage))
  local component_name = buf_name == "" and "[No Name]" or vim.fn.fnamemodify(buf_name, ":t") -- TODO: get [No Name] from vim api? i think there is some option to change this

  return (tabpage_is_active and HIGHLIGHT_SEL or HIGHLIGHT_NORM) .. component_name
end

---@param tabpage integer
---@return string
local component_modified = function(tabpage)
  return vim.api.nvim_buf_get_option(tabpage_get_active_buf(tabpage), "modified") and "[+]" or ""
end

---@param tabpage integer
---@return string
local tabline_make_entry = function(tabpage)
  return component_separator(tabpage)
    .. " "
    .. component_icon(tabpage)
    .. " "
    .. component_name(tabpage)
    .. " "
    .. component_modified(tabpage)
    .. "  "
end

vim.o.tabline = "%!v:lua.require('core.tabline')()"

return function()
  local tabline_builder = ""
  for _, tabpage in ipairs(vim.fn.sort(vim.api.nvim_list_tabpages())) do
    tabline_builder = tabline_builder .. tabline_make_entry(tabpage)
  end

  return tabline_builder .. "%#TablineFill#"
end
