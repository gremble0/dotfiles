---@param tabpage_is_active boolean
---@return string
local component_separator = function(tabpage_is_active)
  return tabpage_is_active and "%#TablineSelSep#▎" or "%#TablineNormSep#▏"
end

---@param tabpage_active_buf integer
---@return string
local component_icon = function(tabpage_active_buf)
  local buf_name = vim.fn.bufname(tabpage_active_buf)
  local buf_ft = vim.api.nvim_buf_get_option(tabpage_active_buf, "ft")
  -- TODO: pcall() ?
  local icon, icon_hl = require("nvim-web-devicons").get_icon(buf_name, buf_ft, { default = true })

  return "%#" .. icon_hl .. "#" .. icon
end

---@param tabpage_active_buf integer
---@return string
local component_name = function(tabpage_active_buf)
  local buf_name = vim.fn.bufname(tabpage_active_buf)
  local component_name
  if buf_name == "" then
    component_name = "[No Name]" -- TODO: get this from vim api? i think there is some option to change this
  else
    component_name = vim.fn.fnamemodify(buf_name, ":t")
  end

  return component_name
end

---@param tabpage_active_buf integer
---@return string
local component_modified = function(tabpage_active_buf)
  if vim.api.nvim_buf_get_option(tabpage_active_buf, "modified") then
    return "[+]" -- TODO: get this from vim api?
  else
    return ""
  end
end

---@param tabpage integer
---@return string
local tabline_make_entry = function(tabpage)
  local tabpage_is_active = tabpage == vim.fn.tabpagenr()
  local buflist = vim.fn.tabpagebuflist(tabpage)
  local winnr = vim.fn.tabpagewinnr(tabpage)

  local tabpage_active_buf
  if type(buflist) == "number" then
    tabpage_active_buf = buflist
  else
    tabpage_active_buf = buflist[winnr]
  end

  return component_separator(tabpage_is_active)
    .. " "
    .. component_icon(tabpage_active_buf)
    .. " "
    .. component_name(tabpage_active_buf)
    .. " "
    .. component_modified(tabpage_active_buf)
    .. "  "
end

vim.o.tabline = "%!v:lua.require('core.tabline')()"

return function()
  local tabline_builder = ""
  for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
    tabline_builder = tabline_builder .. tabline_make_entry(tabpage)
  end

  return tabline_builder .. "%#TablineFill#"
end
