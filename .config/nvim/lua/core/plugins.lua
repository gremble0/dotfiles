---@class ExtendedPackSpec : vim.pack.Spec
---@field dependencies? ExtendedPackSpec[] dependencies needed to load before `spec`
---@field priority? integer higher value means higher priority in terms of plugin load order
---@field setup? fun(): nil function run to setup plugin after installation
---@field build? fun(): nil command ran to build plugin. Runs before setup()

---@type ExtendedPackSpec[]
local specs = {}

for name, _ in vim.fs.dir(vim.fn.stdpath("config") .. "/lua/plugins") do
  local plugin_base_filename = name:gsub(".lua", "")
  local spec = require("plugins." .. plugin_base_filename)
  table.insert(specs, spec)
end

---@type ExtendedPackSpec[]
local specs_flattened = {}

---Flatten all of `spec`s dependencies and put them in `dest`
---@param spec ExtendedPackSpec
---@param dest ExtendedPackSpec[]
local function flatten_dependencies(spec, dest)
  if spec.dependencies then
    for _, dependency in ipairs(spec.dependencies) do
      flatten_dependencies(dependency, dest)
    end
  end
  spec.dependencies = nil
  table.insert(dest, spec)
end

for _, spec in ipairs(specs) do
  flatten_dependencies(spec, specs_flattened)
end

-- NOTE: there is table.sort, but i remember it not working properly here... idk
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

local pack_changed_group = vim.api.nvim_create_augroup("pack_changed_group", {})
for _, spec in ipairs(specs_flattened) do
  if spec.build then
    vim.api.nvim_create_autocmd("PackChanged", {
      group = pack_changed_group,
      callback = function(event)
        -- This doesnt really work, but there isnt really anything we can do about it. Nvim will call this autocmd
        -- before the package has been loaded into the runtime path so any build functions that require the package
        -- its building will error. I dont like it, but we have to find other ways to build such packages. For
        -- treesitter we can just do :TSUpdate after we have started
        if event.data.spec.src == spec.src and (event.data.kind == "update" or event.data.kind == "install") then
          spec.build()
        end
      end,
    })
  end
end

vim.pack.add(specs_flattened)

for _, spec in ipairs(specs_flattened) do
  if spec.setup then
    spec.setup()
  end
end
