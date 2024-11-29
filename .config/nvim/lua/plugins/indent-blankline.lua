-- Add indentation guides even on blank lines
return {
  "lukas-reineke/indent-blankline.nvim",
  opts = {
    indent = {
      char = "▏",
    },
    scope = {
      char = "▏",
      show_start = false,
      show_end = false,
    },
  },
  -- Using opts calls the setup for the wrong version of the plugin so have to manually call it
  config = function(_, opts)
    require("ibl").setup(opts)
  end,
}
