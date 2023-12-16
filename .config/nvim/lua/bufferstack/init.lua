---@class BufferStack
---@field buffers integer[]
local M = {}

--TODO remove killed buffers

---@param buffer integer
function M.add_buffer(buffer)
  M.buffers[#M.buffers + 1] = buffer
end

---@param buffer integer
function M.delete_buffer(buffer)
  print("ASD")
  for i, buf in ipairs(M.buffers) do
    if buf == buffer then
      M.buffers[i] = M.buffers[i + 1]
      return
    end
  end
end

function M.bprevious()
  local last_buffer = M.buffers[#M.buffers]
  for i = #M.buffers, 1, -1 do
    M.buffers[i] = M.buffers[i - 1]
  end

  M.buffers[1] = last_buffer
  vim.api.nvim_set_current_buf(last_buffer)
end

function M.bnext()
  local first_buffer = M.buffers[1]
  for i = 1, #M.buffers do
    M.buffers[i] = M.buffers[i + 1]
  end

  M.buffers[#M.buffers + 1] = first_buffer
  vim.api.nvim_set_current_buf(first_buffer)
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
    callback = function() print(vim.api.nvim_get_current_buf()) end
  })

  -- vim.api.nvim_create_autocmd("BufUnload", {
  --   callback = function() M.delete_buffer(vim.api.nvim_get_current_buf()) end
  -- })
end

return M
