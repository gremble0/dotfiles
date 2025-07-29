-- Preview markdown files with live updates
---@type PluginSpec
return {
  vim_pack_spec = { src = "https://github.com/iamcco/markdown-preview.nvim" },
  --TODO: this doesnt work methinks
  build = function()
    vim.fn["mkdp#util#install"]()
  end,
}
