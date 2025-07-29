-- Theme
---@type ExtendedPackSpec
return {
  src = "https://github.com/gremble0/yellowbeans.nvim",
  priority = 1000,
  setup = function()
    vim.cmd.colorscheme("yellowbeans")
  end,
}
