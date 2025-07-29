local before = os.clock()
require("core.options") -- This needs to be loaded first (sets leader and stuff)
require("core.mappings")
require("core.autocmd")
require("core.usercmd")
require("core.diagnostics")
require("core.plugins")
local after = os.clock()
STARTUP_TIME = after - before
