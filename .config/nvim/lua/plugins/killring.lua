-- [[ Configure killring ]]
local killring = require("killring")
local com = require("core.common")

killring.setup({ buffer_local = true })

com.ks("n", "<leader>gk", killring.open)
