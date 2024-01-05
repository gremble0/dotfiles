-- [[ Configure Telescope ]]
require("telescope").setup {
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
    file_sorter = require("telescope.sorters").get_fuzzy_file,
    generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
    set_env = { ["COLORTERM"] = "truecolor" },
    mappings = {
      i = { ["<esc>"] = require("telescope.actions").close },
    },
  },
}

-- Enable telescope fzf native, if installed
pcall(require("telescope").load_extension, "fzf")

vim.keymap.set("n", "<leader>o", require("telescope.builtin").oldfiles, { desc = "Find recently opened files" })
vim.keymap.set("n", "<leader>f", require("telescope.builtin").find_files,
  { desc = "Find files" })
vim.keymap.set("n", "<leader>b", require("telescope.builtin").buffers,
  { desc = "Find open buffers" })
vim.keymap.set("n", "<leader>z", require("telescope.builtin").current_buffer_fuzzy_find,
  { desc = "Fuzzy find current buffer" })
vim.keymap.set("n", "<leader>d", require("telescope.builtin").diagnostics,
  { desc = "Get diagnostics" })
vim.keymap.set("n", "<leader>gh", require("telescope.builtin").help_tags,
  { desc = "Find help" })
vim.keymap.set("n", "<leader>gr", require("telescope.builtin").live_grep,
  { desc = "Live grep" })
