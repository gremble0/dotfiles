require("core.options")
require("core.mappings")
-- TODO: fix hl group NeogitFilePath

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
  -- My theme
  {
    "gremble0/yellowbeans.nvim",
    priority = 1000,
    config = function()
      vim.cmd.colorscheme("yellowbeans")
    end
  },

  -- Git functionality like magit inside nvim
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "sindrets/diffview.nvim",
      "ibhagwan/fzf-lua",
    },
    config = function ()
      require("plugins.neogit")
    end
  },

  -- Adjusts indentation based on what the file is using
  "tpope/vim-sleuth",

  {
    "gremble0/killring.nvim",
    config = function()
      require("plugins.killring")
    end
  },

  {
    "gremble0/bufferstack.nvim",
    config = function()
      require("plugins.bufferstack")
    end
  },

  -- Add indentation guides even on blank lines
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    config = function ()
      require("plugins.ibl")
    end
  },

  -- Display colors such as #fff inside the terminal
  {
    "norcalli/nvim-colorizer.lua",
    config = function()
      vim.defer_fn(function()
        require("colorizer").attach_to_buffer(0)
      end, 0)
    end
  },

  -- Adds git releated signs to the gutter, as well as utilities for managing changes
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("plugins.gitsigns")
    end
  },

  -- Statusline
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons", opt = true },
    config = function()
      require("plugins.lualine")
    end,
  },

  -- Prettier netrw
  {
    "prichrd/netrw.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons", opt = true },
    config = function()
      require("netrw").setup()
    end,
  },

  -- Keybinds for commenting regions
  {
    "numToStr/Comment.nvim",
    config = function()
      require("plugins.comment")
    end
  },

  -- LSP Configuration & Plugins
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      -- Automatically install LSPs to stdpath for neovim
      {
        "williamboman/mason.nvim",
        config = true,
        opts = { ui = { border = "single" } },
      },
      "williamboman/mason-lspconfig.nvim",

      -- Useful status updates for LSP
      { "j-hui/fidget.nvim", tag = "legacy", opts = {} },

      -- Additional lua configuration
      "folke/neodev.nvim",
    },
    config = function()
      require("plugins.lsp")
    end
  },

  -- Completion
  {
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

  -- Fuzzy Finder (files, lsp, etc)
  {
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

  -- Highlight, edit, and navigate code
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    build = ":TSUpdate",
    config = function()
      require("plugins.treesitter")
    end
  },
}, {
  ui = {
    border = "single"
  }
})
