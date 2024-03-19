-- My own tabline plugin (currently private repo)
return {
  "gremble0/gline.nvim",

  dependencies = "nvim-tree/nvim-web-devicons",

  dev = "~/Code/plugins/gline.nvim",

  config = function()
    require("gline").setup()
  end,
}
