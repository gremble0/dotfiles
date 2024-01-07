-- [[ Configure Telescope ]]
local telescope = require("telescope")
local actions = require("telescope.actions")
local sorters = require("telescope.sorters")
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
    selection_caret = "  ",
    entry_prefix = "  ",
    prompt_prefix = "  ",
    selection_strategy = "reset",
    sorting_strategy = "ascending",
    layout_config = {
      horizontal = {
        height = 0.5,
        width = 0.75,
        prompt_position = "top",
        preview_width = 0.5,
      },
    },
    file_sorter = sorters.get_fuzzy_file,
    generic_sorter = sorters.get_generic_fuzzy_sorter,
    set_env = { ["COLORTERM"] = "truecolor" },
    mappings = {
      i = { ["<esc>"] = actions.close },
    },
  },
})

-- Enable telescope fzf native, if installed
pcall(telescope.load_extension, "fzf")

vim.keymap.set("n", "<leader>o", telescope_builtin.oldfiles, { desc = "Find recently opened files" })
vim.keymap.set("n", "<leader>f", telescope_builtin.find_files, { desc = "Find files" })
vim.keymap.set("n", "<leader>b", telescope_builtin.buffers, { desc = "Find open buffers" })
vim.keymap.set("n", "<leader>z", telescope_builtin.current_buffer_fuzzy_find, { desc = "Fuzzy find current buffer" })
vim.keymap.set("n", "<leader>d", telescope_builtin.diagnostics, { desc = "Get diagnostics" })
vim.keymap.set("n", "<leader>gh", telescope_builtin.help_tags, { desc = "Find help" })
vim.keymap.set("n", "<leader>gr", telescope_builtin.live_grep, { desc = "Live grep" })
