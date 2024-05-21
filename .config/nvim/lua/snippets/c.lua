local ls = require("luasnip")

local s = ls.snippet
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

ls.add_snippets("c", {
  s("fori", fmt("for ({}; {}; {})", { i(1, "int i = 0"), i(2, "i < n"), i(3, "++i") })),
})
