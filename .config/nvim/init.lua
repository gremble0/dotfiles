require("core.options")
require("core.mappings")

local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  -- Git functionality inside nvim
  "tpope/vim-fugitive",

  -- Adjusts indentation based on what the file is using
  "tpope/vim-sleuth",

  -- Add display colors such as #fff inside the terminal
  "norcalli/nvim-colorizer.lua",

  -- File manager
  "nvim-tree/nvim-tree.lua",

  -- Adds git releated signs to the gutter, as well as utilities for managing changes
  "lewis6991/gitsigns.nvim",

  -- Set lualine as statusline
  "nvim-lualine/lualine.nvim",

  -- Add indentation guides even on blank lines
  "lukas-reineke/indent-blankline.nvim",

  -- "<leader>/" to comment visual regions/lines
  "numToStr/Comment.nvim",

  {
    -- LSP Configuration & Plugins
    "neovim/nvim-lspconfig",
    dependencies = {
      -- Automatically install LSPs to stdpath for neovim
      { "williamboman/mason.nvim", config = true },
      "williamboman/mason-lspconfig.nvim",

      -- Useful status updates for LSP
      { "j-hui/fidget.nvim", tag = "legacy", opts = {} },

      -- Additional lua configuration, makes nvim stuff amazing!
      "folke/neodev.nvim",
    },
  },

  {
    -- Autocompletion
    "hrsh7th/nvim-cmp",
    dependencies = {
      -- Snippet Engine & its associated nvim-cmp source
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",

      -- Adds LSP completion capabilities
      "hrsh7th/cmp-nvim-lsp",

      -- Adds a number of user-friendly snippets
      "rafamadriz/friendly-snippets",

      -- Adds icons to cmp window
      "onsails/lspkind.nvim"
    },
  },

  {
    -- Colorscheme
    "metalelf0/jellybeans-nvim",
    dependencies = { "rktjmp/lush.nvim" },
    priority = 1000,
    config = function()
      vim.cmd.colorscheme "jellybeans-nvim"
    end,
  },

  {
    -- Fuzzy Finder (files, lsp, etc)
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    branch = "0.1.x",
  },

  {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "make",
    cond = function()
      return vim.fn.executable "make" == 1
    end,
  },

  {
    -- Highlight, edit, and navigate code
    "nvim-treesitter/nvim-treesitter",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    build = ":TSUpdate",
  },
}, {})

-- [[ Configure gitsigns ]]
require("gitsigns").setup {
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "‾" },
    changedelete = { text = "~" },
  },

  vim.keymap.set("n", "<leader>gp", require("gitsigns").prev_hunk, { desc = "[G]o to [P]revious Hunk" }),
  vim.keymap.set("n", "<leader>gn", require("gitsigns").next_hunk, {  desc = "[G]o to [N]ext Hunk" }),
  vim.keymap.set("n", "<leader>ph", require("gitsigns").preview_hunk, {  desc = "[P]review [H]unk" }),
}

-- [[ Configure Lualine ]]
require("lualine").setup {
    options = {
      component_separators = "",
      section_separators = "",
    },
    sections = {
      lualine_a = { "mode" },
      lualine_b = { "filename" },
      lualine_c = { "branch", "diff" },
      lualine_x = { "diagnostics" },
      lualine_y = { "location" },
      lualine_z = {},
    }
}

-- [[ Configure Indent Blankline ]]
require("indent_blankline").setup {
  char = "│",
  show_trailing_blankline_indent = false,
  vim.cmd.highlight "IndentBlanklineChar guifg=#333333 gui=nocombine"
}

-- [[ Configure Comment ]]
require("Comment").setup {
  toggler = {
    line = "<leader>/",
  },
  opleader = {
    line = "<leader>/",
  }
}

-- [[ Configure Telescope ]]
require("telescope").setup {
  defaults = {
    vimgrep_arguments = {
      "rg",
      "-L",
      "--color=never",
      "--no-heading",
      "--with-filename",
      "--line-number",
      "--column",
      "--smart-case",
    },
    selection_caret = "  ",
    entry_prefix = "  ",
    selection_strategy = "reset",
    sorting_strategy = "ascending",
    layout_config = {
      horizontal = {
        prompt_position = "top",
        preview_width = 0.5,
        results_width = 0.8,
      },
    },
    file_sorter = require("telescope.sorters").get_fuzzy_file,
    generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
    borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
    set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
    mappings = {
      n = { ["q"] = require("telescope.actions").close },
    },
  },
}

-- [[ Configure Nvim Tree ]]
require("nvim-tree").setup {
  vim.keymap.set("n", "<C-n>", ":NvimTreeToggle<CR>", { desc = "Toggle nvim-tree" }),

  disable_netrw = true,
  hijack_netrw = true,
  git = {
    enable = true,
  },
  filesystem_watchers = {
    enable = true,
  },
  actions = {
    open_file = {
      resize_window = true,
    },
  },
  renderer = {
    highlight_git = true,
    -- highlight_opened_files = "none",
    root_folder_label = false,

    icons = {
      show = {
        folder_arrow = true,
        git = false,
      },

      glyphs = {
        default = "",
        symlink = "",
        folder = {
          default = "",
          empty = "",
          empty_open = "",
          open = "",
          symlink = "",
          symlink_open = "",
          arrow_open = "",
          arrow_closed = "",
        },
      },
    },
  },
}

-- Enable telescope fzf native, if installed
pcall(require("telescope").load_extension, "fzf")

vim.keymap.set("n", "<leader>?", require("telescope.builtin").oldfiles, { desc = "[?] Find recently opened files" })
vim.keymap.set("n", "<leader><space>", require("telescope.builtin").buffers, { desc = "[ ] Find existing buffers" })

vim.keymap.set("n", "<leader>ff", require("telescope.builtin").find_files, { desc = "[S]earch [F]iles" })
vim.keymap.set("n", "<leader>fh", require("telescope.builtin").help_tags, { desc = "[S]earch [H]elp" })
vim.keymap.set("n", "<leader>sd", require("telescope.builtin").diagnostics, { desc = "[S]earch [D]iagnostics" })

-- [[ Configure Treesitter ]]
require("nvim-treesitter.configs").setup {
  ensure_installed = { "c", "cpp", "go", "lua", "python", "rust", "tsx", "typescript", "vimdoc", "vim" },

  highlight = { enable = true },
  indent = { enable = true },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<c-space>",
      node_incremental = "<c-space>",
      scope_incremental = "<c-s>",
      node_decremental = "<M-space>",
    },
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["aa"] = "@parameter.outer",
        ["ia"] = "@parameter.inner",
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer",
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer",
      },
    },
    swap = {
      enable = true,
      swap_next = {
        ["<leader>a"] = "@parameter.inner",
      },
      swap_previous = {
        ["<leader>A"] = "@parameter.inner",
      },
    },
  },
}

-- Diagnostic keymaps
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic message" })
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Go to next diagnostic message" })
vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "Open floating diagnostic message" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostics list" })

-- [[ Configure LSP ]]
-- This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = "LSP: " .. desc
    end

    vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
  end

  nmap("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")
  nmap("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")

  nmap("gd", vim.lsp.buf.definition, "[G]oto [D]efinition")
  nmap("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")
  nmap("gI", vim.lsp.buf.implementation, "[G]oto [I]mplementation")
  nmap("<leader>D", vim.lsp.buf.type_definition, "Type [D]efinition")
  -- nmap("<leader>ds", require("telescope.builtin").lsp_document_symbols, "[D]ocument [S]ymbols")
  nmap("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")

  -- See `:help K` for why this keymap
  nmap("K", vim.lsp.buf.hover, "Hover Documentation")
  nmap("<C-k>", vim.lsp.buf.signature_help, "Signature Documentation")

  -- Lesser used LSP functionality
  nmap("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
  nmap("<leader>wa", vim.lsp.buf.add_workspace_folder, "[W]orkspace [A]dd Folder")
  nmap("<leader>wr", vim.lsp.buf.remove_workspace_folder, "[W]orkspace [R]emove Folder")
  nmap("<leader>wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, "[W]orkspace [L]ist Folders")

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
    vim.lsp.buf.format()
  end, { desc = "Format current buffer with LSP" })
end

local servers = {
  clangd = {},
  pyright = {},
  tsserver = {},
  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
}

-- Setup neovim lua configuration
require("neodev").setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

-- Ensure the servers above are installed
local mason_lspconfig = require "mason-lspconfig"

mason_lspconfig.setup {
  ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
  function(server_name)
    require("lspconfig")[server_name].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
    }
  end,
}

-- [[ Configure nvim-cmp ]]
local cmp = require("cmp")
local lspkind = require("lspkind")
local luasnip = require("luasnip")
require("luasnip.loaders.from_vscode").lazy_load()
luasnip.config.setup {}

local cmpborder = {
  { "┌", "CmpBorder" },
  { "─", "CmpBorder" },
  { "┐", "CmpBorder" },
  { "│", "CmpBorder" },
  { "┘", "CmpBorder" },
  { "─", "CmpBorder" },
  { "└", "CmpBorder" },
  { "│", "CmpBorder" },
}

cmp.setup {
  formatting = {
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
      winhighlight = "Normal:CmpPmenu,CursorLine:PmenuSel,Search:PmenuSel",
      scrollbar = false,
    },
    documentation = {
      border = cmpborder,
      winhighlight = "Normal:CmpPmenu",
    },
  },
}

-- Attatch nvim-colorizer to buffer
vim.defer_fn(function()
  require("colorizer").attach_to_buffer(0)
end, 0)
