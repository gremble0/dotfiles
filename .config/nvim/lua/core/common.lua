local M = {}

M.ks = vim.keymap.set
M.kd = vim.keymap.del

function M.win_close()
  vim.api.nvim_win_close(0, true)
end

return M
