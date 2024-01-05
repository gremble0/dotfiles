-- [[ Configures native lsp and integrates nvim-cmp and mason to apply language servers ]]
local lsp = vim.lsp

-- Configure diagnostic floats
vim.diagnostic.config({
  update_in_insert = true,
  float = {
    focusable = false,
    border = "rounded",
    source = "always",
    prefix = "",
  },
})

-- Get user input for renaming the word currently under the cursor
local rename = function()
  local cur_name = vim.fn.expand("<cword>")

  local new_name = vim.fn.input("Rename '" .. cur_name .. "': ")
  if #new_name > 0 and new_name ~= cur_name then
    local params = vim.lsp.util.make_position_params()
    params.newName = new_name
    vim.lsp.buf_request(0, "textDocument/rename", params)
  end
end

local on_attach = function(_, bufnr)
  local telescope_builtin = require("telescope.builtin")

  vim.keymap.set("n", "<leader>rn", rename, { buffer = bufnr, desc = "LSP: Rename" })
  vim.keymap.set("n", "<leader>ca", lsp.buf.code_action, { buffer = bufnr, desc = "LSP: Code action" })

  vim.keymap.set("n", "gd", lsp.buf.definition, { buffer = bufnr, desc = "LSP: Goto definition" })
  vim.keymap.set("n", "gD", lsp.buf.declaration, { buffer = bufnr, desc = "LSP: Goto declaration" })
  vim.keymap.set("n", "gi", lsp.buf.implementation, { buffer = bufnr, desc = "LSP: Goto implementation" })
  vim.keymap.set("n", "gt", lsp.buf.type_definition, { buffer = bufnr, desc = "LSP: Goto type Definition" })

  vim.keymap.set("n", "gr", telescope_builtin.lsp_references, { buffer = bufnr, desc = "LSP: Goto references" })
  vim.keymap.set("n", "gs", telescope_builtin.lsp_dynamic_workspace_symbols,
    { buffer = bufnr, desc = "LSP: Goto workspace symbols" })

  vim.keymap.set("n", "K", lsp.buf.hover, { buffer = bufnr, desc = "LSP: Hover documentation" })
  -- vim.keymap.set("n", "<C-k>", lsp.buf.signature_help, { buffer = bufnr, desc = "LSP: Signature Documentation" })

  vim.keymap.set("n", "<leader>wa", lsp.buf.add_workspace_folder,
    { buffer = bufnr, desc = "LSP: Add workspace folder" })
  vim.keymap.set("n", "<leader>wr", lsp.buf.remove_workspace_folder,
    { buffer = bufnr, desc = "LSP: Remove workspace folder" })

  vim.keymap.set("n", "<leader>wl", function()
    print(vim.inspect(lsp.buf.list_workspace_folders()))
  end, { buffer = bufnr, desc = "List workspace folders" })

  vim.keymap.set("n", "<leader>mt", lsp.buf.format, { buffer = bufnr, desc = "LSP: Format current buffer" })
end

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
  border = "rounded",
})

-- List of configured language servers
local servers = {
  clangd = {},
  pyright = {},
  tsserver = {},
  cssls = {},
  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
      diagnostics = {
        globals = { "vim" }
      },
    },
  },
  gopls = {},
  bashls = {},
  jdtls = {
    cmd = {
      "java",
      "-Declipse.application=org.eclipse.jdt.ls.core.id1",
      "-Dosgi.bundles.defaultStartLevel=4",
      "-Declipse.product=org.eclipse.jdt.ls.core.product",
      "-Xms1g",
      "--add-modules=ALL-SYSTEM",
      "--add-opens", "java.base/java.util=ALL-UNNAMED",
      "--add-opens", "java.base/java.lang=ALL-UNNAMED",
      "-jar",
      "~/.local/share/nvim/mason/share/jdtls/org.eclipse.equinox.launcher.jar",
      "-configuration", "~/.local/share/nvim/mason/share/jdtls/config/config.ini",
      "-data", "~/.local/share/nvim/java"
    },
    settings = {
      java = {
        signatureHelp = { enabled = true },
        import = { enabled = false },
        rename = { enabled = true }
      }
    },
  },
}

-- Setup neovim lua configuration
require("neodev").setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

-- Ensure the servers above are installed
require("mason-lspconfig").setup {
  ensure_installed = vim.tbl_keys(servers),
}

require("mason-lspconfig").setup_handlers {
  function(server_name)
    require("lspconfig")[server_name].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
    }
  end,
}
