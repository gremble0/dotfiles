local ks = vim.keymap.set

--- NORMAL MODE KEYBINDS
do
  -- Navigation
  do
    -- Windows
    ks("n", "<C-h>", "<C-w>h", { desc = "Move to window to the left" })
    ks("n", "<C-j>", "<C-w>j", { desc = "Move to window below" })
    ks("n", "<C-k>", "<C-w>k", { desc = "Move to window above" })
    ks("n", "<C-l>", "<C-w>l", { desc = "Move to window to the right" })

    -- Buffers
    ks("n", "<C-c>", ":close!<CR>", { desc = "Close buffer", silent = true })
    ks("n", "<C-q>", "<C-^>", { desc = "Alternate file" })

    -- Tabs
    ks("n", "<leader>tn", ":tabnew<CR>", { desc = "Make new tab", silent = true })
    ks("n", "<leader>tc", ":tabclose<CR>", { desc = "Close tab", silent = true })

    -- Quickfix-/Location- list
    ks("n", "<leader>qo", ":copen<CR>", { desc = "Open quickfix list", silent = true })
    ks("n", "<leader>qc", ":cclose<CR>", { desc = "Close quickfix list", silent = true })
    ks("n", "<leader>lo", ":lopen<CR>", { desc = "Open location list", silent = true })
    ks("n", "<leader>lc", ":lclose<CR>", { desc = "Close location list", silent = true })
    ks("n", "[q", ":cclose<CR>:cprevious<CR>", { desc = "Go to previous quickfix error", silent = true })
    ks("n", "]q", ":cclose<CR>:cnext<CR>", { desc = "Go to next quickfix error", silent = true })

    -- Clear highlights with escape
    ks("n", "<Esc>", ":noh<CR>", { desc = "Clear highlights" })

    -- Remap for dealing with word wrap
    ks("n", "k", "v:count == 0 ? 'gk' : 'k'", { desc = "Move up with word wrapping",  silent = true })
    ks("n", "j", "v:count == 0 ? 'gj' : 'j'", { desc = "Move down with word wrapping", silent = true })
  end

  -- Lsp stuff
  do
    local buf = vim.lsp.buf

    ks("n", "grd", buf.definition, { desc = "LSP: Goto definition" })
    ks("n", "grD", buf.declaration, { desc = "LSP: Goto declaration" })

    ks("n", "K", function()
      buf.hover({ border = "rounded" })
    end, { desc = "LSP: Hover documentation" })
    ks({ "n", "i", "s" }, "<C-s>", function()
      buf.signature_help({ border = "rounded" })
    end, { desc = "LSP: Signature Documentation" })
  end

  -- Open lazy
  ks("n", "<leader>z", ":Lazy<CR>", { desc = "Open lazy", silent = true })

  -- Copy stuff from current file (useful for debugging with gdb)
  ks("n", "<leader>cp", function()
    vim.fn.system(string.format("echo %s | xclip -selection clipboard", vim.fn.expand("%")))
  end, {})
  ks("n", "<leader>cl", function()
    vim.fn.system(string.format("echo %s:%s | xclip -selection clipboard", vim.fn.expand("%"), vim.fn.line(".")))
  end, {})
end

--- VISUAL MODE KEYBINDS
do
  -- Move lines
  ks("x", "J", ":m '>+1<CR>gv=gv", { desc = "Move selected lines down", silent = true })
  ks("x", "K", ":m '<-2<CR>gv=gv", { desc = "Move selected lines up", silent = true })
end

--- INSERT AND COMMAND MODE KEYBINDS
do
  -- Navigation
  ks({ "i", "c" }, "<C-h>", "<Left>", { desc = "Move left" })
  ks({ "i", "c" }, "<C-j>", "<Down>", { desc = "Move down" })
  ks({ "i", "c" }, "<C-k>", "<Up>", { desc = "Move up" })
  ks({ "i", "c" }, "<C-l>", "<Right>", { desc = "Move right" })
  ks({ "i", "c" }, "<C-a>", "<Home>", { desc = "Move to the start of the line" })
  ks({ "i", "c" }, "<C-e>", "<End>", { desc = "Move to the end of the line" })
  ks({ "i", "c" }, "<M-f>", "<C-Right>", { desc = "Move forward word" })
  ks({ "i", "c" }, "<M-b>", "<C-Left>", { desc = "Move back word" })
  ks({ "i", "c" }, "<C-b>", "<Left>", { desc = "Move left" })
  ks({ "i", "c" }, "<C-f>", "<Right>", { desc = "Move right" })
end
