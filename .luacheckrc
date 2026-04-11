local function find_lua_globals()
  local prefixes = { ".", "..", "../..", "../../..", "../../../.." }
  for _, prefix in ipairs(prefixes) do
    local path = prefix .. "/nvim/.config/nvim/lua/lua_globals.lua"
    local file = io.open(path, "r")
    if file then
      file:close()
      return path
    end
  end
  error("could not locate nvim/.config/nvim/lua/lua_globals.lua")
end

local lua_globals = dofile(find_lua_globals())

globals = lua_globals.globals
read_globals = lua_globals.read_globals
