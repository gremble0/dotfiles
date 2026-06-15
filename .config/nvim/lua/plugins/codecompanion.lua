---@type ExtendedPackSpec
-- return {
--   src = "https://github.com/olimorris/codecompanion.nvim",
--   dependencies = {
--     { src = "https://github.com/nvim-lua/plenary.nvim" },
--     { src = "https://github.com/nvim-treesitter/nvim-treesitter" },
--     { src = "https://github.com/zbirenbaum/copilot.lua" },
--   },
--   setup = function()
--     require("codecompanion").setup()
--   end,
-- }
--

---@type ExtendedPackSpec
return {
  src = "https://github.com/CopilotC-Nvim/CopilotChat.nvim",
  dependencies = {
    { src = "https://github.com/nvim-lua/plenary.nvim" },
    {
      src = "https://github.com/zbirenbaum/copilot.lua",
      setup = function()
        require("copilot").setup()
      end,
    },
  },
  build = function()
    vim
      .system({ "make", "tiktoken" }, { cwd = vim.fn.stdpath("data") .. "/site/pack/core/opt/CopilotChat.nvim" })
      :wait()
  end,
  setup = function()
    require("CopilotChat").setup()
  end,
}
