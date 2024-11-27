-- Theme
return {
  "gremble0/yellowbeans.nvim",
  priority = 1000,
  -- For local development
  dir = "~/Code/plugins/yellowbeans.nvim",
  config = function()
    vim.cmd.colorscheme("yellowbeans")
  end,
}
