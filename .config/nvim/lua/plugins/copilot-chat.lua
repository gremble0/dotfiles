---@type ExtendedPackSpec
return {
  src = "https://github.com/CopilotC-Nvim/CopilotChat.nvim",
  dependencies = {
    { src = "https://github.com/nvim-lua/plenary.nvim" },
  },
  build = function()
    vim
      .system({ "make", "tiktoken" }, { cwd = vim.fn.stdpath("data") .. "/site/pack/core/opt/CopilotChat.nvim" })
      :wait()
  end,
  setup = function()
    local cc = require("CopilotChat")
    cc.setup()

    vim.keymap.set("n", "<leader>cm", cc.select_model, { desc = "Select copilot model" })
    vim.keymap.set("n", "<leader>co", cc.open, { desc = "Open CopilotChat window" })
  end,
}
