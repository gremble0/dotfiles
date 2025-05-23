-- TUI picker menus
local get_buffer_path = function()
  -- If in an oil buffer get the path from oil
  if vim.bo.filetype == "oil" then
    local ok, oil = pcall(require, "oil")
    if ok then
      return oil.get_current_dir()
    end
  end

  return require("telescope.utils").buffer_dir()
end

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
  opts = function()
    local action_state = require("telescope.actions.state")
    local actions = require("telescope.actions")
    local sorters = require("telescope.sorters")

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

    return {
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
          mappings = { i = { ["<C-c>"] = delete_buffer, ["<C-CR>"] = tabnew } },
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
    }
  end,
  config = function(_, opts)
    local telescope = require("telescope")
    telescope.setup(opts)
    telescope.load_extension("fzf")
  end,
  keys = {
    {
      "<leader>b",
      function()
        require("telescope.builtin").buffers()
      end,
      desc = "Telescope find open buffers",
      silent = true,
    },
    {
      "<leader>ff",
      function()
        require("telescope.builtin").find_files({ hidden = true, no_ignore = true, no_ignore_parent = true })
      end,
      desc = "Telescope find files",
      silent = true,
    },
    {
      "<leader>fw",
      function()
        require("telescope.builtin").find_files({
          cwd = get_buffer_path(),
          hidden = true,
          no_ignore = true,
          no_ignore_parent = true,
        })
      end,
      desc = "Telescope find files",
    },
    {
      "<leader>fg",
      function()
        require("telescope.builtin").git_files()
      end,
      desc = "Telescope git files",
    },
    {
      "<leader>fh",
      function()
        require("telescope.builtin").help_tags()
      end,
      desc = "Telescope find help",
    },
    {
      "<leader>rr",
      function()
        require("telescope.builtin").live_grep()
      end,
      desc = "Telescope live grep",
    },
    {
      "<leader>rw",
      function()
        require("telescope.builtin").live_grep({ cwd = get_buffer_path() })
      end,
      desc = "Telescope live grep",
    },
  },
  cmd = "Telescope",
}
