local M = {}

-- Delete buffer if only open in one window, otherwise close it.
-- (smart mix between :bd and :close)
M.close_win_or_buffer = function()
  local curbuf = vim.api.nvim_get_current_buf()
  local curwin = vim.api.nvim_get_current_win()

  for _, win in ipairs(vim.api.nvim_list_wins()) do
    if win == curwin then
      goto continue
    end

    if vim.api.nvim_win_get_buf(win) == curbuf then
      vim.api.nvim_win_close(curwin, false)
      return
    end

    ::continue::
  end

  vim.api.nvim_buf_delete(curbuf, { force = false })
end

return M
