-- Add indentation guides even on blank lines
return {
  "lukas-reineke/indent-blankline.nvim",

  config = function()
    require("ibl").setup({
      indent = {
        highlight = { "VertSplit" },
        char = "▏",
      },
      scope = {
        highlight = { "Comment" },
        char = "▏",
        show_start = false,
        show_end = false,
      },
    })
  end,
}
