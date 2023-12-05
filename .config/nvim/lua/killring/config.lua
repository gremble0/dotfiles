--- @alias OpenKind "telescope" | "split"
-- TODO: implement UI ^

--- @class KillRingConfig
--- @field values string
--- @field max_size integer
--- @field open_kind string
local M = {}

--- @param opts? table<string, any>
--- @return KillRingConfig
function M.get_config(opts)
  local config = M.get_default_config()

  if opts ~= nil then
    for k, v in pairs(opts) do
      config[k] = v
    end
  end

  return config
end

--- @return KillRingConfig
function M.get_default_config()
  return {
    max_size = 10,
    open_kind = "telescope", -- < TODO: implement open UI
  }
end

return M
