  local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")
local com = require("core.common")
local sorters = require("telescope.sorters")
local telescope = require("telescope")
local telescope_builtin = require("telescope.builtin")

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
          ["<C-g>"] = actions.delete_buffer + actions.close,
        }
      }
    }
  }
})

-- actions.delete_buffer doesn't work like expected
local function delete_buffer(prompt_bufnr)
  local selection = action_state.get_selected_entry()
  actions.close(prompt_bufnr)
  vim.api.nvim_buf_delete(selection.bufnr, {})
end

local function buffers_with_delete()
  telescope_builtin.buffers({
    attach_mappings = function(_, map)
      map("i", "<C-c>", delete_buffer)
      map("n", "<C-c>", delete_buffer)
      return true
    end,
  })
end

-- Enable telescope fzf native, if installed
pcall(telescope.load_extension, "fzf")

com.ks("n", "<leader>o", telescope_builtin.oldfiles, { desc = "Find recently opened files" })
com.ks("n", "<leader>f", telescope_builtin.find_files, { desc = "Find files" })
com.ks("n", "<leader>b", buffers_with_delete, {noremap = true, silent = true})
com.ks("n", "<leader>z", telescope_builtin.current_buffer_fuzzy_find, { desc = "Fuzzy find current buffer" })
com.ks("n", "<leader>gh", telescope_builtin.help_tags, { desc = "Find help" })
com.ks("n", "<leader>gr", telescope_builtin.live_grep, { desc = "Live grep" })

-- ["<RET>"] = actions.file_vsplit,
