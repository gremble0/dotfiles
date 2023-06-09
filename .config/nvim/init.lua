require("core.options")
require("core.mappings")
require("core.colors")

-- Initialize lazy
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

-- Import plugins using lazy
require("lazy").setup({
  -- Git functionality inside nvim
  "tpope/vim-fugitive",

  -- Adjusts indentation based on what the file is using
  "tpope/vim-sleuth",

  {
    -- Add display colors such as #fff inside the terminal
    "norcalli/nvim-colorizer.lua",
    config = function()
      vim.defer_fn(function()
        require("colorizer").attach_to_buffer(0)
      end, 0)
    end
  },

  {
    -- File manager
    "nvim-tree/nvim-tree.lua",
    config = function()
      require("plugins.nvimtree")
    end
  },

  {
    -- Adds git releated signs to the gutter, as well as utilities for managing changes
    "lewis6991/gitsigns.nvim",
    config = function()
      require("plugins.gitsigns")
    end
  },

  {
    -- Set lualine as statusline
    "nvim-lualine/lualine.nvim",
    config = function()
      require("plugins.lualine")
    end
  },

  {
    -- Add indentation guides even on blank lines
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("plugins.indent_blankline")
    end
  },

  {
    -- "<leader>/" to comment visual regions/lines
    "numToStr/Comment.nvim",
    config = function()
      require("plugins.comment")
    end
  },

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
    config = function()
      require("plugins.lsp")
    end
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
    config = function()
      require("plugins.cmp")
    end
  },

  -- {
  --   -- Colorscheme
  --   "metalelf0/jellybeans-nvim",
  --   dependencies = { "rktjmp/lush.nvim" },
  --   priority = 1000,
  --   config = function()
  --     vim.cmd.colorscheme "jellybeans-nvim"
  --   end,
  -- },

  {
    -- Fuzzy Finder (files, lsp, etc)
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    branch = "0.1.x",
    config = function()
      require("plugins.telescope")
    end
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
    config = function()
      require("plugins.treesitter")
    end
  },
}, {})
