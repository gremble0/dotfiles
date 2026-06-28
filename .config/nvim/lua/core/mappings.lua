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

    -- Clear highlights with escape
    ks("n", "<Esc>", ":noh<CR>", { desc = "Clear highlights" })

    -- Remap for dealing with word wrap
    ks("n", "k", "v:count == 0 ? 'gk' : 'k'", { desc = "Move up with word wrapping", silent = true, expr = true })
    ks("n", "j", "v:count == 0 ? 'gj' : 'j'", { desc = "Move down with word wrapping", silent = true, expr = true })
  end

  -- Lsp stuff
  do
    local lb = vim.lsp.buf

    -- Missing from defaults
    ks("n", "grd", lb.definition, { desc = "vim.lsp.buf.definition()" })

    -- In defaults, but only in insert mode
    ks({ "n", "i", "s" }, "<C-s>", lb.signature_help, { desc = "vim.lsp.buf.signature_help()" })

    local make_autoformat_autocmd = function()
      return vim.api.nvim_create_autocmd("BufWritePre", {
        group = vim.api.nvim_create_augroup("AutoFormat", { clear = false }),
        pattern = "*",
        callback = function(opts)
          lb.format({ bufnr = opts.buf, timeout_ms = 3000 })
        end,
      })
    end

    --- Enable automatic formatting by default - set to nil to disable by default
    ---@type integer?
    local autoformat_cmd = make_autoformat_autocmd()

    ks("n", "<leader>mt", lb.format, { desc = "Format current buffer" })

    ks("n", "<leader>me", function()
      autoformat_cmd = autoformat_cmd or make_autoformat_autocmd()
    end, { desc = "Enable autoformatting" })

    ks("n", "<leader>md", function()
      if autoformat_cmd then
        vim.api.nvim_del_autocmd(autoformat_cmd)
        autoformat_cmd = nil
      end
    end, { desc = "Disable autoformatting", silent = true })
  end

  local get_clipboard = function()
    local XDG_SESSION_TYPE = os.getenv("XDG_SESSION_TYPE")
    if XDG_SESSION_TYPE == "wayland" and vim.fn.executable("wl-copy") == 1 then
      return "wl-copy"
    elseif XDG_SESSION_TYPE == "x11" and vim.fn.executable("xclip") == 1 then
      return "xclip -selection clipboard"
    end

    error("Could not find system clipboard")
  end

  -- Copy stuff from current file (useful for debugging with gdb)
  ks("n", "<leader>cp", function()
    vim.fn.system(string.format("echo %s | %s", vim.fn.expand("%"), get_clipboard()))
  end, { desc = "Copy current file to clipboard" })
  ks("n", "<leader>cl", function()
    vim.fn.system(string.format("echo %s:%s | %s", vim.fn.expand("%"), vim.fn.line("."), get_clipboard()))
  end, { desc = "Copy current line to clipboard" })
end

--- VISUAL MODE KEYBINDS
do
  -- Move lines
  ks("x", "J", ":m '>+1<CR>gv", { desc = "Move selected lines down", silent = true })
  ks("x", "K", ":m '<-2<CR>gv", { desc = "Move selected lines up", silent = true })
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
