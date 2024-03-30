return {
  "gremble0/yellowbeans.nvim",

  priority = 1000,
  dev = "~/Code/plugins/yellowbeans.nvim",

  config = function()
    vim.cmd.colorscheme("yellowbeans")
  end,
}
