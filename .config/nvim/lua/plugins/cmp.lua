-- [[ Configure nvim-cmp ]]
local cmp = require("cmp")
local lspkind = require("lspkind")
local luasnip = require("luasnip")
require("luasnip.loaders.from_vscode").lazy_load()
luasnip.config.setup {}

-- Define our own colors for cmp
vim.api.nvim_set_hl(0, "CmpMenu", { fg = "White", bg = "#141414" }) -- Supposed to be #151515 but some weird rendering shit makes theme display 141414 instead so we use that instead here
vim.api.nvim_set_hl(0, "CmpMenuBg", { fg = "Black", bg = "#141414" })
vim.api.nvim_set_hl(0, "CmpMenuSel", { fg = "Black", bg = "#e1b655" })

-- local get_hl = vim.api.nvim_get_hl_id_by_name
-- Maybe useful function instead of hardcoding? only returns id not color code

-- Colors for the "kind" field in the cmpmenu
vim.api.nvim_set_hl(0, "CmpItemKindClass", { fg = "#8fbedc" })
vim.api.nvim_set_hl(0, "CmpItemKindInterface", { fg = "#8fbedc" })
vim.api.nvim_set_hl(0, "CmpItemKindKeyword", { fg = "#b2d1f0" })
vim.api.nvim_set_hl(0, "CmpItemKindMethod", { fg = "#e1b655" })
vim.api.nvim_set_hl(0, "CmpItemKindFunction", { fg = "#e1b655" })
vim.api.nvim_set_hl(0, "CmpItemKindVariable", { fg = "#c5b5ee" })
vim.api.nvim_set_hl(0, "CmpItemKindModule", { fg = "#c5b5ee" })
vim.api.nvim_set_hl(0, "CmpItemKindField", { fg = "#c5b5ee" })

local cmpborder = {
  { "┌", "CmpMenuBg" },
  { "─", "CmpMenuBg" },
  { "┐", "CmpMenuBg" },
  { "│", "CmpMenuBg" },
  { "┘", "CmpMenuBg" },
  { "─", "CmpMenuBg" },
  { "└", "CmpMenuBg" },
  { "│", "CmpMenuBg" },
}

cmp.setup {
  formatting = {
    fields = { "kind", "menu", "abbr" },
    format = lspkind.cmp_format(),
  },
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-e>"] = cmp.mapping.close(),
    ["<C-Space>"] = cmp.mapping.complete {},
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" }),
  },
  sources = {
    { name = "nvim_lsp" },
  },
  window = {
    completion = {
      border = cmpborder,
      side_padding = 1,
      col_offset = -3,
      winhighlight = "Normal:CmpMenu,CursorLine:CmpMenuSel,Search:None",
      scrollbar = false,
    },
    documentation = {
      border = cmpborder,
      side_padding = 1,
      winhighlight = "Normal:CmpMenu,Search:None",
    },
  },
}
