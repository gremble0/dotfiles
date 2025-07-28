-- Theme
---@type PluginSpec
return {
  spec = {
    src = "https://github.com/gremble0/yellowbeans.nvim",
  },
  setup = {
    priority = 1000,
    setup = function()
      vim.cmd.colorscheme("yellowbeans")
    end,
  },
}
