local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")
local com = require("core.common")
local sorters = require("telescope.sorters")
local telescope = require("telescope")
local telescope_builtin = require("telescope.builtin")

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
      mappings = {
        i = {
          ["<C-c>"] = delete_buffer,
        }
      }
    }
  }
})

-- Enable telescope fzf native, if installed
pcall(telescope.load_extension, "fzf")

com.ks("n", "<leader>o", telescope_builtin.oldfiles, { desc = "Find recently opened files" })
com.ks("n", "<leader>f", telescope_builtin.find_files, { desc = "Find files" })
com.ks("n", "<leader>b", telescope_builtin.buffers, { desc = "Find open buffers" })
com.ks("n", "<leader>z", telescope_builtin.current_buffer_fuzzy_find, { desc = "Fuzzy find current buffer" })
com.ks("n", "<leader>gh", telescope_builtin.help_tags, { desc = "Find help" })
com.ks("n", "<leader>gr", telescope_builtin.live_grep, { desc = "Live grep" })

-- ["<RET>"] = actions.file_vsplit,
