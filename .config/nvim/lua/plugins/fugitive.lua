local com = require("core.common")

local fugitive_group = vim.api.nvim_create_augroup("FugitiveCustom", {})

com.ks("n", "<leader>gt", ":vertical Git<CR>", { silent = true })

vim.api.nvim_create_autocmd("FileType", {
  group = fugitive_group,
  pattern = "fugitive",
  callback = function()
    com.kd("n", "p", { buffer = 0 })
    com.kd("n", "P", { buffer = 0 })

    com.ks("n", "q", com.delete_cur_buf, { desc = "Delete current buffer" })
    com.ks("n", "pu", ":Git push<CR>", { desc = "Git push", silent = true })
    com.ks("n", "Pu", ":Git pull<CR>", { desc = "Git pull", silent = true })
  end,
})
