local ks = vim.keymap.set

local delete_buf_opts = { desc = "Delete current buffer", buffer = 0 }
local file_open_rules_group = vim.api.nvim_create_augroup("FileOpenRules", {})

local delete_cur_buf = function()
  vim.api.nvim_buf_delete(0, { force = true })
end

-- Use 'q' to quickly close these filetypes
vim.api.nvim_create_autocmd("FileType", {
  group = file_open_rules_group,
  pattern = { "help", "qf", "gitcommit", "fugitive", "oil" },
  callback = function()
    ks("n", "q", delete_cur_buf, delete_buf_opts)
  end,
})

-- Enter insert mode in gitcommit buffers
vim.api.nvim_create_autocmd("FileType", {
  group = file_open_rules_group,
  pattern = "gitcommit",
  callback = function()
    vim.cmd("startinsert")
  end,
})

-- Dont automatically continue comments
vim.api.nvim_create_autocmd("FileType", {
  group = file_open_rules_group,
  pattern = "*",
  callback = function()
    vim.opt.formatoptions:remove("r")
    vim.opt.formatoptions:remove("o")
  end,
})
