local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")
local sorters = require("telescope.sorters")
local telescope = require("telescope")

-- actions.delete_buffer doesn't work like expected unless prompt is closed
-- before calling nvim_buf_delete()
local delete_buffer = function(prompt_bufnr)
  local current_picker = action_state.get_current_picker(prompt_bufnr)
  actions.close(prompt_bufnr)
  current_picker:delete_selection(function(selection)
    local force = vim.api.nvim_buf_get_option(selection.bufnr, "buftype") == "terminal"
    local ok = pcall(vim.api.nvim_buf_delete, selection.bufnr, { force = force })
    return ok
  end)
end

telescope.setup({
  defaults = {
    vimgrep_arguments = {
      "rg",
      "-L",
      "--color=never",
      "--no-heading",
      "--hidden",
      "--with-filename",
      "--line-number",
      "--column",
      "--smart-case",
      "--no-ignore-vcs",
    },
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
      },
    },
  },
  pickers = {
    buffers = {
      show_all_buffers = true,
      sort_lastused = true,
      mappings = { i = { ["<C-c>"] = delete_buffer } },
    },
  },
  extensions = {
    ["ui-select"] = {
      require("telescope.themes").get_cursor(),
    },
  },
})

-- Enable some extensions if installed properly
pcall(telescope.load_extension, "fzf")
pcall(telescope.load_extension, "ui-select")
