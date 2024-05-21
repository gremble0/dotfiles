local ls = require("luasnip")

local s = ls.snippet
local i = ls.insert_node

local fmt = require("luasnip.extras.fmt").fmt

ls.add_snippets("all", {
  -- s("fori", fmt("<%= {} %>{}", { i(1), i(0) })),
  s("fori", fmt("for ({}; {}; {})", { i(0), i(1), i(2) })),
})
