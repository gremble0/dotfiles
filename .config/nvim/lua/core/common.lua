local M = {}

M.ks = vim.keymap.set
M.kd = vim.keymap.del

function M.delete_cur_buf()
  vim.api.nvim_buf_delete(0, { force = true })
end

return M
