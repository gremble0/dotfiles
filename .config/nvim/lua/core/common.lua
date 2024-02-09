local M = {}

M.ks = vim.keymap.set

function M.delete_cur_buf()
  vim.api.nvim_buf_delete(0, {})
end

return M
