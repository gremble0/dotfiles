---@param tabpage integer
---@return string
local tabpage_make_component_separator = function(tabpage)
  if tabpage == vim.fn.tabpagenr() then
    return "%#TablineSelSep#▎%#TablineSel#"
  else
    return "%#TablineNormSep#▏%#TablineNorm#"
  end
end

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
  local icon, icon_hl = require("nvim-web-devicons").get_icon(buf_name, buf_ft, { default = true })

  return "%#" .. icon_hl .. "#" .. icon
end

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
  local buf_name_abbr = vim.fn.fnamemodify(buf_name, ":t")

  return "%#TablineNorm" .. buf_name_abbr .. "#"
end

---@param tabpage integer
---@return string
local tabpage_make_component = function(tabpage)
  return tabpage_make_component_separator(tabpage)
    .. tabpage_make_component_icon(tabpage)
    .. tabpage_make_component_name(tabpage)
end

_G.tabline = function()
  local tabline_builder = ""
  for _, tabpage in ipairs(vim.api.nvim_list_tabpages()) do
    tabline_builder = tabline_builder .. " " .. tabpage_make_component(tabpage) .. "  "
  end

  return tabline_builder .. "%#TablineFill#"
end

vim.o.tabline = "%!v:lua.tabline()"

vim.keymap.set("n", "<leader>tn", ":tabnew<CR>")
