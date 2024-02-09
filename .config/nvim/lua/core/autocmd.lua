local com = require("core.common")

local ks_opts = { noremap = true, silent = true, buffer = 0 }
local file_open_rules_group = vim.api.nvim_create_augroup("FileOpenRules", {})

local function delete_cur_buf()
  vim.api.nvim_buf_delete(0, {})
end

vim.api.nvim_create_autocmd("FileType", {
  group = file_open_rules_group,
  pattern = "help",
  callback = function()
    com.ks("n", "q", delete_cur_buf, ks_opts)
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  group = file_open_rules_group,
  pattern = "gitcommit",
  callback = function()
    vim.cmd("startinsert")

    com.ks("n", "q", delete_cur_buf, ks_opts)
  end,
})
