-- Completion
return {
  "hrsh7th/nvim-cmp",
  dependencies = {
    -- Adds icons to cmp window
    "onsails/lspkind.nvim",

    -- Cmp for commandline
    "hrsh7th/cmp-cmdline",
  },
  config = function()
    local cmp = require("cmp")
    local lspkind = require("lspkind")

    cmp.setup({
      formatting = {
        expandable_indicator = false,
        fields = { "kind", "abbr", "menu" },
        format = function(entry, vim_item)
          local kind = lspkind.cmp_format({ mode = "symbol_text", maxwidth = 35 })(entry, vim_item)
          local strings = vim.split(kind.kind, "%s", { trimempty = true })
          kind.kind = strings[1] or ""
          kind.menu = strings[2] and ("(" .. strings[2] .. ")") or ""

          return kind
        end,
      },

      snippet = {
        expand = function(args)
          vim.snippet.expand(args.body)
        end,
      },

      mapping = {
        ["<C-n>"] = cmp.mapping(function(_)
          if cmp.visible() then
            cmp.select_next_item()
          else
            cmp.complete()
          end
        end, { "i", "s" }),
        ["<C-p>"] = cmp.mapping(function(_)
          if cmp.visible() then
            cmp.select_prev_item()
          else
            cmp.complete()
          end
        end, { "i", "s" }),
        ["<Tab>"] = cmp.mapping(function(callback)
          if vim.snippet.active({ direction = 1 }) then
            vim.snippet.jump(1)
          else
            callback()
          end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(callback)
          if vim.snippet.active({ direction = -1 }) then
            vim.snippet.jump(-1)
          else
            callback()
          end
        end, { "i", "s" }),
        ["<C-u>"] = cmp.mapping.scroll_docs(-4),
        ["<C-d>"] = cmp.mapping.scroll_docs(4),
        ["<C-g>"] = cmp.mapping.close(),
        ["<CR>"] = cmp.mapping.confirm(),
      },

      sources = {
        {
          name = "nvim_lsp",
          entry_filter = function(entry, _)
            -- Filter out LSP snippets
            return entry:get_kind() ~= require("cmp.types").lsp.CompletionItemKind.Snippet
          end,
        },
        { name = "luasnip", priority = 1000 },
      },

      window = {
        completion = cmp.config.window.bordered({
          border = "rounded",
          scrolloff = 2,
        }),
        documentation = cmp.config.window.bordered({
          border = "rounded",
          scrolloff = 2,
        }),
      },
    })

    cmp.setup.cmdline(":", {
      mapping = {
        ["<C-n>"] = cmp.mapping(function(_)
          if cmp.visible() then
            cmp.select_next_item()
          else
            cmp.complete()
          end
        end, { "c" }),
        ["<C-p>"] = cmp.mapping(function(_)
          if cmp.visible() then
            cmp.select_prev_item()
          else
            cmp.complete()
          end
        end, { "c" }),
        ["<Tab>"] = cmp.mapping(function(_) end, { "c" }),
        ["<S-Tab>"] = cmp.mapping(function(_) end, { "c" }),
      },

      sources = cmp.config.sources({ { name = "cmdline" } }),
    })
  end,
}
