---@class BufferStack
---@field buffers integer[]
---@field buffers_index integer
local M = {}

---@param buffer integer
function M.add_buffer(buffer)
  -- check if we already have buffer
  for i, buf in ipairs(M.buffers) do
    if buf == buffer then
      M.buffers_index = i
      return
    end
  end

  M.buffers[#M.buffers + 1] = buffer
  M.buffers_index = #M.buffers
end

-- function M.garbage_collect()
--   for i, buf in ipairs(M.buffers) do
--     if not vim.api.nvim_buf_is_loaded(buf) then
--       for j = i, #M.buffers do
--         M.buffers[j] = M.buffers[j + 1]
--       end
--     end
--   end
-- end

function M.bnext()
  if M.buffers_index == #M.buffers then
    M.buffers_index = 1
  else
    M.buffers_index = M.buffers_index + 1
  end

  vim.api.nvim_set_current_buf(M.buffers[M.buffers_index])

  M.show()
end

function M.bprevious()
  if M.buffers_index == 1 then
    M.buffers_index = #M.buffers
  else
    M.buffers_index = M.buffers_index - 1
  end

  vim.api.nvim_set_current_buf(M.buffers[M.buffers_index])

  M.show()
end

function M.show()
  for _, buf in ipairs(M.buffers) do
    local bufname
    if vim.api.nvim_buf_is_loaded(buf) then
      bufname = vim.api.nvim_buf_get_name(buf)
    else
      bufname = "*DELETED*"
    end
    print(buf, bufname)
  end
  print("---------")
end

function M.setup()
  -- TODO autocmd group
  M.buffers = {}
  M.buffers_index = 1

  local buffer_stack_group = vim.api.nvim_create_augroup("BufferStack", {})
  vim.api.nvim_create_autocmd("BufWinEnter", {
    group = buffer_stack_group,
    callback = function() M.add_buffer(vim.api.nvim_get_current_buf()) end
  })
end

return M
