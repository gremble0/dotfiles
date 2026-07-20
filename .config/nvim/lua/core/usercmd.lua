---NOTE: Do not call this function with longer strings as it will generate a LOT of combinations
---scaling exponentially with the length of the string (O(2 ^ n))
---@param user_command string
local generate_case_combinations = function(user_command)
  ---@type string[]
  local combinations = {}

  for i = 0, (2 ^ #user_command) - 1 do
    -- User commands must start with a capital letter. Only generate combinations for letters after the first.
    local combination = user_command:sub(1, 1):upper()
    for j = 2, #user_command do
      local bit = math.floor(i / (2 ^ (j - 1))) % 2
      if bit == 1 then
        combination = combination .. user_command:sub(j, j):upper()
      else
        combination = combination .. user_command:sub(j, j):lower()
      end
    end
    table.insert(combinations, combination)
  end

  return combinations
end

-- Make usercommands for all possible case combinations of these commonly used commands
local commands = { "w", "wq", "wa", "wqa", "q", "qa", "e" }

for _, command in ipairs(commands) do
  for _, combo in ipairs(generate_case_combinations(command)) do
    vim.api.nvim_create_user_command(combo, function(opts)
      vim.cmd(command .. (opts.bang and "!" or ""))
    end, { bang = true })
  end
end
