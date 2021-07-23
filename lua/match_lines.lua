local inspect = require("inspect")

local function gmatch(s, pattern)
  local matches = {}

  for match in string.gmatch(s, pattern) do
    table.insert(matches, match)
  end

  return matches
end

local function parse_lines(s)
  local lines = string.gmatch(s, "[^\r\n]+")
  local matches = {}

  for line in lines do
    local match = string.match(line, "\n?%s*| ?([^\n]*)")
    table.insert(matches, match)
  end

  return matches
end

local input = [[
  | import antigravity
  |
  | def hello():
  |     print('hello')
  |
  |     print('world')
]]

print(inspect(parse_lines(input)))
