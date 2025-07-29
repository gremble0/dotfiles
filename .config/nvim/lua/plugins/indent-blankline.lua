-- Add indentation guides even on blank lines
---@type PluginSpec
return {
  src = "https://github.com/lukas-reineke/indent-blankline.nvim",
  setup = function()
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
