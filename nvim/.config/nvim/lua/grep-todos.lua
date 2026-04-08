----------------------------------------------------------------------
-- Module
----------------------------------------------------------------------
local M = {}

----------------------------------------------------------------------
-- Defaults
----------------------------------------------------------------------
local default_opts = {
	search = "TODO:|FIX:",
	no_esc = true,
	prompt = "TODOs> ",
	rg_opts = "--column --line-number --no-heading --color=always --smart-case -g '!*archive*'",
}

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
function M.grep(opts)
	local merged = vim.tbl_deep_extend("force", default_opts, opts or {})
	require("fzf-lua").grep(merged)
end

return M
