-- [[ Configure nvim-cmp ]]
local cmp = require("cmp")
local lspkind = require("lspkind")
local luasnip = require("luasnip")
require("luasnip.loaders.from_vscode").lazy_load()
luasnip.config.setup()

vim.keymap.set({ "i", "s", "n" }, "<C-/>", function() luasnip.expand() end, { silent = true })
vim.keymap.set({ "i", "s", "n" }, "<C-;>", function() luasnip.jump(1) end, { silent = true })
vim.keymap.set({ "i", "s", "n" }, "<C-,>", function() luasnip.jump(-1) end, { silent = true })

local has_words_before = function()
  unpack = unpack or table.unpack
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local cmpborder = { "┌",  "─",  "┐",  "│",  "┘",  "─",  "└",  "│" }

cmp.setup {
  formatting = {
    fields = { "kind", "abbr", "menu" },
    format = function(entry, vim_item)
      local kind = lspkind.cmp_format({ mode = "symbol_text", maxwidth = 35 })(entry, vim_item)
      local strings = vim.split(kind.kind, "%s", { trimempty = true })
      kind.kind = " " .. (strings[1] or "")
      kind.menu = " (" .. (strings[2] or "") .. ")"

      return kind
    end
  },
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ["<C-n>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      elseif has_words_before() then
        cmp.complete()
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<C-p>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<C-u>"] = cmp.mapping.scroll_docs(-4),
    ["<C-d>"] = cmp.mapping.scroll_docs(4),
    ["<C-g>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace },
  },
  sources = {
    { name = "nvim_lsp" },
  },
  window = {
    completion = cmp.config.window.bordered({
      border = cmpborder,
      side_padding = 0,
      col_offset = -3,
      winhighlight = "Normal:Pmenu,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None",
      scrollbar = true,
      scrolloff = 2,
    }),
    documentation = cmp.config.window.bordered({
      border = cmpborder,
      side_padding = 0,
      winhighlight = "Normal:Pmenu,FloatBorder:FloatBorder,Search:None",
      scrolloff = 2,
    }),
  },
}
