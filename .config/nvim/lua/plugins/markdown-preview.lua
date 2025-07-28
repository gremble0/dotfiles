-- Preview markdown files with live updates
---@type PluginSpec
return {
  spec = { src = "https://github.com/iamcco/markdown-preview.nvim" },
  ---TODO: cmd?
  -- cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
  ---TODO: filetype?
  -- ft = { "markdown" },
  ---TODO: whatever this is
  -- build = function()
  --   vim.fn["mkdp#util#install"]()
  -- end,
}
