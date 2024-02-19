require("core")

-- Initialize lazy
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Import plugins using lazy
require("lazy").setup({
  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft = { "markdown" },
    build = function() vim.fn["mkdp#util#install"]() end,
  },

  -- Theme
  {
    "gremble0/yellowbeans.nvim",
    priority = 1000,
    config = function()
      vim.cmd.colorscheme("yellowbeans")
    end,
  },

  {
    "tpope/vim-fugitive",
    keys = { { "<leader>gt", ":vertical Git<CR>", desc = "Open fugitive", silent = true } },
    cmd = "Git",
    config = function()
      require("plugins.fugitive")
    end,
  },

  -- Pretty UI for notifications
  {
    "rcarriga/nvim-notify",
    config = function()
      require("plugins.notify")
    end,
  },

  -- Pretty UI to help with keybinds
  {
    "folke/which-key.nvim",
    config = function()
      require("plugins.whichkey")
    end,
  },

  -- Adjusts indentation based on what the file is using
  "tpope/vim-sleuth",

  -- Fix unintuitive :bprevious and :bnext behavior
  {
    "gremble0/bufferstack.nvim",
    config = function()
      require("plugins.bufferstack")
    end,
  },

  -- Add indentation guides even on blank lines
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("plugins.ibl")
    end,
  },

  -- Display colors such as #fff inside the terminal
  {
    "norcalli/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup({ "*" }, { names = false })
    end,
  },

  -- Adds git releated signs to the gutter, as well as utilities for managing changes
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("plugins.gitsigns")
    end,
    keys = {
      { "<leader>hp", ":Gitsigns prev_hunk<CR>", desc = "Goto previous git hunk", silent = true },
      { "<leader>hn", ":Gitsigns next_hunk<CR>", desc = "Goto next git hunk", silent = true },
      { "<leader>hv", ":Gitsigns preview_hunk_inline<CR>",  desc = "Preview git hunk", silent = true },
    },
    lazy = false,
  },

  -- Statusline
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons", opt = true },
    config = function()
      require("plugins.lualine")
    end,
  },

  -- DWIM File navigation and editing
  {
    "stevearc/oil.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("plugins.oil")
    end,
    keys = { { "<C-e>", ":Oil<CR>", desc = "Open file explorer (oil)", silent = true } },
    cmd = "Oil",
  },

  -- Keybinds for commenting regions
  {
    "numToStr/Comment.nvim",
    config = function()
      require("plugins.comment")
    end,
  },

  -- LSP Configuration & Plugins
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      -- Automatically install LSPs to stdpath for neovim
      {
        "williamboman/mason.nvim",
        dependencies = "williamboman/mason-lspconfig.nvim",
        config = true,
        opts = { ui = { border = "rounded" } },
      },

      -- Useful status updates for LSP
      { "j-hui/fidget.nvim", tag = "legacy", opts = {} },

      -- Additional lua configuration
      "folke/neodev.nvim",

      -- Better java LSP functionality
      "mfussenegger/nvim-jdtls",
    },
    config = function()
      require("plugins.lsp")
    end,
  },

  -- Pretty UI for diagnostics
  {
    "folke/trouble.nvim",
    config = function()
      require("trouble").setup({ action_keys = { cancel = {} } })
    end,
    keys = { { "<leader>t", ":Trouble<CR>", desc = "Open trouble split", silent = true } },
    cmd = "Trouble",
  },

  -- Completion
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      -- Cmp for commandline
      "hrsh7th/cmp-cmdline",

      -- Snippet Engine & its associated nvim-cmp source
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",

      -- Adds LSP completion capabilities
      "hrsh7th/cmp-nvim-lsp",

      -- Adds icons to cmp window
      "onsails/lspkind.nvim"
    },
    config = function()
      require("plugins.cmp")
    end,
  },

  -- Generate documentation
  {
    "danymat/neogen",
    dependencies = "nvim-treesitter/nvim-treesitter",
    opts = { snippet_engine = "luasnip" },
    keys = {
      { "<leader>gd", ":Neogen<CR>", desc = "Generate documentation", silent = true },
    },
    cmd = "Neogen",
  },

  -- Fuzzy Finder (files, lsp, etc)
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        cond = function() return vim.fn.executable("make") == 1 end,
      },
    },
    config = function()
      require("plugins.telescope")
    end,
    keys = {
      { "<leader>o",  ":Telescope oldfiles<CR>",   desc = "Find recently opened files", silent = true },
      { "<leader>f",  ":Telescope find_files<CR>", desc = "Find files",                 silent = true },
      { "<leader>b",  ":Telescope buffers<CR>",    desc = "Find open buffers",          silent = true },
      { "<leader>gh", ":Telescope help_tags<CR>",  desc = "Find help",                  silent = true },
      { "<leader>gr", ":Telescope live_grep<CR>",  desc = "Live grep",                  silent = true },
    },
    cmd = "Telescope",
  },

  -- Highlight, edit, and navigate code
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    build = ":TSUpdate",
    config = function()
      require("plugins.treesitter")
    end,
  },
}, {
  ui = {
    border = "rounded"
  }
})
