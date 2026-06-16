-- Highlight, edit, and navigate code
---@type ExtendedPackSpec
return {
  src = "https://github.com/nvim-treesitter/nvim-treesitter",
  version = "main",
  setup = function()
    vim.api.nvim_create_autocmd("FileType", {
      callback = function(event)
        local lang = vim.treesitter.language.get_lang(vim.bo[event.buf].filetype) or vim.bo[event.buf].filetype

        -- Install the treesitter parser if not already installed.
        -- Only start treesitter after we have ensured its installed.
        local treesitter = require("nvim-treesitter")
        local treesitter_config = require("nvim-treesitter.config")

        if not vim.tbl_contains(treesitter.get_available(), lang) then
          return
        end

        if vim.tbl_contains(treesitter_config.get_installed(), lang) then
          vim.treesitter.start(event.buf)
        else
          treesitter.install({ lang }):await(function()
            vim.treesitter.start(event.buf)
          end)
        end
      end,
    })
  end,
  build = function()
    require("nvim-treesitter.install").update()
  end,
}
