---@class BufferStack
---@field buffers integer[]
local M = {}

--TODO remove killed buffers

---@param buffer integer
function M.add_buffer(buffer)
  -- check if we already have buffer
  for _, buf in ipairs(M.buffers) do
    if buf == buffer then
      return
    end
  end

  M.buffers[#M.buffers + 1] = buffer
end

---@param buffer integer
function M.delete_buffer(buffer)
  for i, buf in ipairs(M.buffers) do
    if buf == buffer then
      for j = i, #M.buffers do
        M.buffers[j] = M.buffers[j + 1]
      end
      break
    end
  end
end

function M.bprevious()
  local last_buffer = M.buffers[#M.buffers]

  for i = #M.buffers, 1, -1 do
    M.buffers[i] = M.buffers[i - 1]
  end

  M.buffers[1] = last_buffer
  vim.api.nvim_set_current_buf(M.buffers[1])
end

function M.bnext()
  local first_buffer = M.buffers[1]

  print(#M.buffers)
  for i = 1, #M.buffers do
    M.buffers[i] = M.buffers[i + 1]
  end
  print(#M.buffers)

  M.buffers[#M.buffers + 1] = first_buffer
  vim.api.nvim_set_current_buf(M.buffers[1])
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
  vim.api.nvim_create_autocmd("BufEnter", {
    callback = function() M.add_buffer(vim.api.nvim_get_current_buf()) end
  })

  vim.api.nvim_create_autocmd("BufUnload", {
    callback = function() M.delete_buffer(vim.api.nvim_get_current_buf()) end
  })
end

return M
