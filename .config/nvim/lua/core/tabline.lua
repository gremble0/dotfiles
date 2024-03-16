---@param tabpage integer
---@return string
local tabpage_make_component_separator = function(tabpage)
  if tabpage == vim.fn.tabpagenr() then
    return "%#TablineSelSep#▎"
  else
    return "%#TablineNormSep#▏"
  end
end

--TODO: change params to reduce duplication?
---@param tabpage integer
---@return string
local tabpage_make_component_icon = function(tabpage)
  local buflist = vim.fn.tabpagebuflist(tabpage)
  local winnr = vim.fn.tabpagewinnr(tabpage)

  local tabpage_active_buf
  if type(buflist) == "number" then
    tabpage_active_buf = buflist
  else
    tabpage_active_buf = buflist[winnr]
  end

  local buf_name = vim.fn.bufname(tabpage_active_buf)
  local buf_ft = vim.api.nvim_buf_get_option(tabpage_active_buf, "ft")
  -- TODO: pcall() ?
  local icon, icon_hl = require("nvim-web-devicons").get_icon(buf_name, buf_ft, { default = true })

  return "%#" .. icon_hl .. "#" .. icon
end

---@param tabpage integer
---@return string
local tabpage_make_component_name = function(tabpage)
  local buflist = vim.fn.tabpagebuflist(tabpage)
  local winnr = vim.fn.tabpagewinnr(tabpage)

  local tabpage_active_buf
  if type(buflist) == "number" then
    tabpage_active_buf = buflist
  else
    tabpage_active_buf = buflist[winnr]
  end

  local buf_name = vim.fn.bufname(tabpage_active_buf)
  local component_name
  if buf_name == "" then
    component_name = "[No Name]"
  else
    component_name = vim.fn.fnamemodify(buf_name, ":t")
  end

  local component_hl
  if tabpage == vim.fn.tabpagenr() then
    component_hl = "%#TablineSel#"
  else
    component_hl = "%#TablineNorm#"
  end

  return component_hl .. component_name
end

local tabpage_make_component_modified = function(tabpage)
  local buflist = vim.fn.tabpagebuflist(tabpage)
  local winnr = vim.fn.tabpagewinnr(tabpage)

  local tabpage_active_buf
  if type(buflist) == "number" then
    tabpage_active_buf = buflist
  else
    tabpage_active_buf = buflist[winnr]
  end

  if vim.api.nvim_buf_get_option(tabpage_active_buf, "modified") then
    return "[+]"
  else
    return ""
  end
end

---@param tabpage integer
---@return string
local tabpage_make_component = function(tabpage)
  return tabpage_make_component_separator(tabpage)
    .. " "
    .. tabpage_make_component_icon(tabpage)
    .. " "
    .. tabpage_make_component_name(tabpage)
    .. " "
    .. tabpage_make_component_modified(tabpage)
    .. "  "
end

vim.o.tabline = "%!v:lua.require('core.tabline')()"

return function()
  local tabline_builder = ""
  for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
    tabline_builder = tabline_builder .. tabpage_make_component(tabpage)
  end

  return tabline_builder .. "%#TablineFill#"
end
