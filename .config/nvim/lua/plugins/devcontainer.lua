---@type ExtendedPackSpec
return {
  src = "https://codeberg.org/esensar/nvim-dev-container",
  setup = function()
    require("devcontainer").setup({})
  end,
}
