return {
  "nvim-telescope/telescope.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",

    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      cond = function()
        return vim.fn.executable("make") == 1
      end,
    },
  },
  config = function()
    local action_state = require("telescope.actions.state")
    local actions = require("telescope.actions")
    local sorters = require("telescope.sorters")
    local telescope = require("telescope")

    local delete_buffer = function(prompt_bufnr)
      local current_picker = action_state.get_current_picker(prompt_bufnr)
      current_picker:delete_selection(function(selection)
        vim.api.nvim_buf_delete(selection.bufnr, { force = true })
      end)
    end

    local tabnew = function(prompt_bufnr)
      local selection = action_state.get_selected_entry()
      actions.close(prompt_bufnr)
      -- try filename, if nil - fallback to [1] (selection text)
      vim.cmd.tabnew(selection.filename or selection[1])
    end

    telescope.setup({
      defaults = {
        selection_caret = " ",
        entry_prefix = " ",
        prompt_prefix = "❯ ",
        selection_strategy = "reset",
        sorting_strategy = "ascending",
        layout_config = {
          horizontal = {
            height = 0.65,
            width = 0.75,
            prompt_position = "top",
            preview_width = 0.5,
          },
        },
        file_sorter = sorters.get_fuzzy_file,
        generic_sorter = sorters.get_generic_fuzzy_sorter,
        mappings = {
          i = {
            ["<esc>"] = actions.close,
            ["<C-l>"] = false,
            ["<C-s>"] = actions.file_split,
          },
        },
      },
      pickers = {
        buffers = {
          show_all_buffers = true,
          sort_lastused = true,
          mappings = { i = { ["<C-c>"] = delete_buffer } },
        },
        find_files = {
          mappings = { i = { ["<C-CR>"] = tabnew } },
        },
        git_files = {
          mappings = { i = { ["<C-CR>"] = tabnew } },
        },
        live_grep = {
          mappings = { i = { ["<C-CR>"] = tabnew } },
        },
      },
    })

    telescope.load_extension("fzf")
  end,
  keys = {
    { "<leader>o", ":Telescope oldfiles<CR>", desc = "Telescope find old files", silent = true },
    { "<leader>b", ":Telescope buffers<CR>", desc = "Telescope find open buffers", silent = true },
    {
      "<leader>f",
      ":lua require('telescope.builtin').find_files({ hidden = true, no_ignore = true, no_ignore_parent = true })<CR>",
      desc = "Telescope find files",
      silent = true,
    },
    { "<leader>gf", ":Telescope git_files<CR>", desc = "Telescope git files", silent = true },
    { "<leader>gh", ":Telescope help_tags<CR>", desc = "Telescope find help", silent = true },
    {
      "<leader>gr",
      ":lua require('telescope.builtin').live_grep({ hidden = true, no_ignore = true, no_ignore_parent = true })<CR>",
      desc = "Telescope live grep",
      silent = true,
    },
  },
  cmd = "Telescope",
}
