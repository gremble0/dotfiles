---@class ExtendedPackSpec : vim.pack.Spec
---@field dependencies? ExtendedPackSpec[] dependencies needed to load before `spec`
---@field setup? fun(): nil function run to setup plugin after installation
---@field priority? integer higher value means higher priority in terms of plugin load order
---@field build? fun(): nil command ran to build plugin. Runs before setup()
---@field cond? fun(): boolean condition to check before running build()

---@type ExtendedPackSpec[]
local specs = {}

for name, _ in vim.fs.dir(vim.fn.stdpath("config") .. "/lua/plugins") do
  local plugin_base_filename = name:gsub(".lua", "")
  local spec = require("plugins." .. plugin_base_filename)
  table.insert(specs, spec)
end

---@type ExtendedPackSpec[]
local specs_flattened = {}

---@param spec ExtendedPackSpec
local function flatten_dependencies(spec)
  if spec.dependencies then
    for _, dependency in ipairs(spec.dependencies) do
      flatten_dependencies(dependency)
    end
  end
  table.insert(specs_flattened, spec)
end

for _, spec in ipairs(specs) do
  flatten_dependencies(spec)
end

-- Sort by priority - preserving dependency order
for i, spec in ipairs(specs_flattened) do
  if spec.priority then
    local insert_index = 1
    while specs_flattened[insert_index].priority and specs_flattened[insert_index].priority > spec.priority do
      insert_index = insert_index + 1
    end
    table.remove(specs_flattened, i)
    table.insert(specs_flattened, insert_index, spec)
  end
end

vim.pack.add(specs_flattened)
for _, spec in ipairs(specs_flattened) do
  if spec.build and spec.cond and spec.cond() then
    spec.build()
  end
  if spec.setup then
    spec.setup()
  end
end
