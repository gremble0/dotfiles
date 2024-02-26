-- Display colors such as #fff inside the terminal
return {
  "norcalli/nvim-colorizer.lua",

  config = function()
    require("colorizer").setup({ "*" }, { names = false })
  end,
}
