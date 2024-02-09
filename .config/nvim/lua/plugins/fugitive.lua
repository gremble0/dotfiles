local com = require("core.common")

local ks_opts = { noremap = true, silent = true, buffer = 0 }
local fugitive_group = vim.api.nvim_create_augroup("FugitiveCustom", {})

com.ks("n", "<leader>gt", ":vertical Git<CR>", { silent = true })

vim.api.nvim_create_autocmd("FileType", {
  group = fugitive_group,
  pattern = "fugitive",
  callback = function()
    com.kd("n", "p", { buffer = 0 })
    com.kd("n", "P", { buffer = 0 })

    com.ks("n", "q", com.delete_cur_buf, ks_opts)
    com.ks("n", "pu", ":echo 'git push' | Git! push<CR>", ks_opts)
    com.ks("n", "Pu", ":echo 'git pull' | Git! pull<CR>", ks_opts)
  end,
})
