require("core")

---@class PluginSetup
---@field setup fun(): nil
---@field priority? integer higher value means higher priority in terms of plugin load order

---@class PluginSpec
---@field spec vim.pack.Spec spec passed to vim.pack.add()
---@field dependencies? vim.pack.Spec[] dependencies for `spec`. Also passed to vim.pack.add()
---@field setup? PluginSetup

---@type vim.pack.Spec[]
local specs = {}
---@type PluginSetup[]
local setups = {}

for name, _ in vim.fs.dir(vim.fn.stdpath("config") .. "/lua/plugins") do
  local plugin_base_filename = name:gsub(".lua", "")
  ---@type PluginSpec
  local spec = require("plugins." .. plugin_base_filename)
  table.insert(specs, spec.spec)
  if spec.dependencies then
    for _, dependency in ipairs(spec.dependencies) do
      table.insert(specs, dependency)
    end
  end
  table.insert(setups, spec.setup)
end

-- Sort by priority
-- table.sort(setups, function(plugin1, plugin2)
--   if plugin1.priority and plugin2.priority then
--     return plugin1.priority > plugin2.priority
--   else
--     return plugin2.priority and false or true
--   end
-- end)

vim.pack.add(specs)
for _, setup in ipairs(setups) do
  if setup.setup then
    setup.setup()
  end
end
