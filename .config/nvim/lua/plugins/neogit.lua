-- [[ Configure Neogit ]]
local neogit = require("neogit")

neogit.setup({
  vim.keymap.set("n", "<leader>gt", neogit.open, { desc = "Open neogit" }),

  disable_hint = true,
  kind = "vsplit",
  graph_style = "unicode",
  commit_editor = {
    kind = "split",
  },
  commit_view = {
    kind = "split",
    verify_commit = os.execute("which gpg") == 0,
  },
  commit_select_view = {
    kind = "split",
  },
  log_view = {
    kind = "split",
  },
  reflog_view = {
    kind = "split",
  },
  mappings = {
    popup = { -- Change to emacs keybinds
      ["p"] = "PushPopup",
      ["P"] = "PullPopup",
    }
  },
  integrations = {
    telescope = true,
    fzf_lua = true,
    diffview = true,
  },
  signs = {
    hunk = { "", "" },
    item = { "", "" },
    section = { "", "" },
  },
})
