-- [[ Configures native lsp and integrates nvim-cmp and mason to apply language servers ]]
-- Define function that will be called whenever a language server attatches to a buffer
local lsp = vim.lsp

local rename = function()
  local cur_name = vim.fn.expand("<cword>")

  local new_name = vim.fn.input("rename " .. cur_name .. ": ")
  if #new_name > 0 and new_name ~= cur_name then
    local params = vim.lsp.util.make_position_params()
    params.newName = new_name
    vim.lsp.buf_request(0, "textDocument/rename", params)
  end
end

local on_attach = function(_, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = "LSP: " .. desc
    end

    vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
  end

  nmap("<leader>rn", rename, "[R]e[n]ame")
  nmap("<leader>ca", lsp.buf.code_action, "[C]ode [A]ction")

  nmap("gd", lsp.buf.definition, "[G]oto [D]efinition")
  nmap("gD", lsp.buf.declaration, "[G]oto [D]eclaration")
  nmap("gi", lsp.buf.implementation, "[G]oto [I]mplementation")
  nmap("gt", lsp.buf.type_definition, "[G]oto [T]ype Definition")

  nmap("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")
  nmap("gs", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[G]oto Workspace [S]ymbols")

  nmap("K", lsp.buf.hover, "Hover Documentation")
  nmap("<C-k>", lsp.buf.signature_help, "Signature Documentation")

  nmap("<leader>wa", lsp.buf.add_workspace_folder, "[W]orkspace [A]dd Folder")
  nmap("<leader>wr", lsp.buf.remove_workspace_folder, "[W]orkspace [R]emove Folder")
  nmap("<leader>wl", function()
    print(vim.inspect(lsp.buf.list_workspace_folders()))
  end, "[W]orkspace [L]ist Folders")

  vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
    lsp.buf.format()
  end, { desc = "Format current buffer with LSP" })
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
        globals = { 'vim' }
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

-- vim.diagnostic.config({})
