require("jdtls").start_or_attach({
  cmd = {
    "java",

    "-Declipse.application=org.eclipse.jdt.ls.core.id1",
    "-Dosgi.bundles.defaultStartLevel=4",
    "-Declipse.product=org.eclipse.jdt.ls.core.product",
    "-Dlog.protocol=true",
    "-Dlog.level=ALL",
    "-Xmx1g",
    "--add-modules=ALL-SYSTEM",
    "--add-opens",
    "java.base/java.util=ALL-UNNAMED",
    "--add-opens",
    "java.base/java.lang=ALL-UNNAMED",

    "-jar",
    "~/.local/share/nvim/mason/packages/jdtls/plugins/org.eclipse.equinox.launcher_1.6.900.v20240613-2009.jar",

    "-configuration",
    "~/.local/share/nvim/mason/packages/jdtls/config_linux",

    "-data",
    "~/.cache/jdtls-workspace/" .. vim.fn.fnamemodify(vim.fn.getcwd(), ":p:h:t"),
  },

  root_dir = vim.fs.root(0, { ".git", "mvnw", "gradlew" }),

  settings = {
    java = {},
  },

  init_options = {
    bundles = {},
  },
})
