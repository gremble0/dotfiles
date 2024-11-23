-- Add indentation guides even on blank lines
return {
  "lukas-reineke/indent-blankline.nvim",

  -- Using opts calls the setup for the wrong version of the plugin so have to manually call it
  config = function()
    require("ibl").setup({
      indent = {
        char = "▏",
      },
      scope = {
        char = "▏",
        show_start = false,
        show_end = false,
      },
    })
  end,
}
