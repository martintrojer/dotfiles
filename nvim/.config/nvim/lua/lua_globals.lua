----------------------------------------------------------------------
-- Module
----------------------------------------------------------------------
local M = {}

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
-- Globals we both read from and write to (`vim.opt.foo = bar`,
-- `hs.window.animationDuration = 0`, etc.). Listing them as
-- `read_globals` makes luacheck flag any sub-field assignment as
-- "setting read-only field" — which is wrong for all of these. We expose
-- them via `globals` only; consumers who want a stricter read-only set
-- can build it themselves.
M.globals = {
	"vim",
	"hs",
	"MiniDiff",
	"MiniGit",
	"MiniStatusline",
	"MiniTrailspace",
}

-- Kept as an empty list so existing `.luacheckrc` files that reference
-- `M.read_globals` (e.g. via `read_globals = lua_globals.read_globals`)
-- continue to load without errors. The lua_ls server config in `lsp.lua`
-- still uses `M.globals` directly for the diagnostics.globals list.
M.read_globals = {}

return M
