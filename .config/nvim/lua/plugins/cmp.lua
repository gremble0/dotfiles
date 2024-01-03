-- [[ Configure nvim-cmp and luasnip ]]
local cmp = require("cmp")
local lspkind = require("lspkind")
local luasnip = require("luasnip")
require("luasnip.loaders.from_vscode").lazy_load()
luasnip.config.setup()

cmp.setup {
  formatting = {
    expandable_indicator = false,
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
    ["<C-n>"] = cmp.mapping(function(_)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        cmp.complete()
      end
    end, { "i", "s" }),
    ["<C-p>"] = cmp.mapping(function(_)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        cmp.complete()
      end
    end, { "i", "s" }),
    ["<Tab>"] = cmp.mapping(function(callback)
      if luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        callback()
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(callback)
      if luasnip.expand_or_jumpable() then
        luasnip.jump(-1)
      else
        callback()
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
      border = "rounded",
      side_padding = 0,
      col_offset = -3,
      winhighlight = "Normal:Pmenu,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None",
      scrollbar = true,
      scrolloff = 2,
    }),
    documentation = cmp.config.window.bordered({
      border = "rounded",
      side_padding = 0,
      winhighlight = "Normal:Pmenu,FloatBorder:FloatBorder,Search:None",
      scrolloff = 2,
    }),
  },
}
