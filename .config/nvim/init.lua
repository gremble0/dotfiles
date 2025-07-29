require("core")

---@class PluginSpec
---@field vim_pack_spec vim.pack.Spec spec passed to vim.pack.add()
---@field dependencies? PluginSpec[] dependencies needed to load before `spec`
---@field setup? fun(): nil
---@field priority? integer higher value means higher priority in terms of plugin load order

---@type PluginSpec[]
local specs = {}

for name, _ in vim.fs.dir(vim.fn.stdpath("config") .. "/lua/plugins") do
  local plugin_base_filename = name:gsub(".lua", "")
  ---@type PluginSpec
  local spec = require("plugins." .. plugin_base_filename)
  table.insert(specs, spec)
end

-- Sort by priority
table.sort(specs, function(plugin1, plugin2)
  local priority_a = plugin1.priority or 0
  local priority_b = plugin2.priority or 0
  return priority_a > priority_b
end)

---@type vim.pack.Spec[]
local vim_pack_specs = {}
---@type (fun(): nil)[]
local setups = {}

---@param spec PluginSpec
local function dfs_setup(spec)
  if spec.dependencies then
    for _, dependency in ipairs(spec.dependencies) do
      dfs_setup(dependency)
    end
  end
  table.insert(vim_pack_specs, spec.vim_pack_spec)
  if spec.setup then
    table.insert(setups, spec.setup)
  end
end

for _, spec in ipairs(specs) do
  dfs_setup(spec)
end

vim.pack.add(vim_pack_specs)
for _, setup in ipairs(setups) do
  setup()
end
