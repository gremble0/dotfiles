local config = require("killring.config")

--- @class KillRing
--- @field config KillRingConfig
--- @field cur_size integer
local M = {}

--- @param value string
function M:add_to_kill_ring(value)
  self.cur_size = self.cur_size + 1
  self[self.cur_size] = value
  print("YO")
end

--- @param opts? table<string, any>
function M.setup(opts)
  M.config = config.get_config(opts)
  M.cur_size = 0

  vim.api.nvim_create_autocmd("TextYankPost", {
    callback = M:add_to_kill_ring(vim.fn.getreg('"'))
  })
end

return M
