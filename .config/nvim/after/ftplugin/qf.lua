local remove_qf_item = function()
  local curqfidx = vim.fn.line(".")
  local qfall = vim.fn.getqflist()

  if #qfall == 0 then
    return
  end

  table.remove(qfall, curqfidx)
  vim.fn.setqflist(qfall, "r")

  vim.cmd.copen()

  local new_idx = curqfidx < #qfall and curqfidx or math.max(curqfidx - 1, 1)

  local winid = vim.fn.win_getid()
  vim.api.nvim_win_set_cursor(winid, { new_idx, 0 })
end

vim.cmd.packadd("cfilter")
vim.keymap.set("n", "dd", remove_qf_item, { desc = "Remove current entry from quickfix list", buffer = 0 })
