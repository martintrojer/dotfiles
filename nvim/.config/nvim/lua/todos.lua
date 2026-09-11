-- TODO helpers: grep TODOs/FIXes/IDEAs

----------------------------------------------------------------------
-- Module
----------------------------------------------------------------------
local M = {}

----------------------------------------------------------------------
-- Defaults
----------------------------------------------------------------------
-- Tags grepped together by M.grep. Filter further inside the fzf popup
-- by typing e.g. `TODO:` / `IDEA:` / `FIX:`. rg runs once.
local tags = { "TODO", "FIX", "IDEA" }

local default_grep_opts = {
	search = table.concat(
		vim.tbl_map(function(t)
			return t .. ":"
		end, tags),
		"|"
	),
	no_esc = true,
	prompt = "Notes> ",
	rg_opts = "--column --line-number --no-heading --color=always --smart-case -g '!*archive*'",
}

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
function M.grep(opts)
	local merged = vim.tbl_deep_extend("force", default_grep_opts, opts or {})
	require("fzf-lua").grep(merged)
end

return M
