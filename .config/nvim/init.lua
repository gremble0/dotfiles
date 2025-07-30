local before = os.clock()

require("core.options") -- This needs to be loaded first (sets leader and stuff)
require("core.mappings")
require("core.autocmd")
require("core.usercmd")
require("core.diagnostics")
require("core.plugins")

local startup_time = os.clock() - before
vim.api.nvim_create_user_command("StartupTime", function()
  print(startup_time)
end, {})
