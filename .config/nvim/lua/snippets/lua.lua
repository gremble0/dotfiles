local ls = require("luasnip")

local s = ls.snippet
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

ls.add_snippets("lua", {
  s(
    "fori",
    fmt(
      [[
      for {}, {}, {} do
        {}
      end
      ]],
      { i(1, "i = 1"), i(2, "i < n"), i(3, "1"), i(4) }
    )
  ),

  s(
    "fore",
    fmt(
      [[
      for {} in {} do
        {}
      end
      ]],
      { i(1, "item"), i(2, "iterator"), i(3, "body") }
    )
  ),
})
