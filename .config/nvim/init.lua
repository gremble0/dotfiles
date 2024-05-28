-- Require basic config
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
---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)

-- Import plugins using lazy
require("lazy").setup({
  spec = "plugins",
  ui = {
    border = "rounded",
    backdrop = 100,
  },
  change_detection = { enabled = false },
  ---@diagnostic disable-next-line: assign-type-mismatch
  dev = { path = "~/Code/plugins" },
  performance = {
    rtp = {
      disabled_plugins = {
        "gzip",
        "matchit",
        "matchparen",
        "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
})
