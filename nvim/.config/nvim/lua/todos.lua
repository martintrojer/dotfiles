-- TODO helpers: grep TODOs/FIXes and toggle markdown todo states

----------------------------------------------------------------------
-- Module
----------------------------------------------------------------------
local M = {}

----------------------------------------------------------------------
-- Defaults
----------------------------------------------------------------------
local default_grep_opts = {
	search = "TODO:|FIX:",
	no_esc = true,
	prompt = "TODOs> ",
	rg_opts = "--column --line-number --no-heading --color=always --smart-case -g '!*archive*'",
}

----------------------------------------------------------------------
-- Transitions
----------------------------------------------------------------------
local transitions = {
	{ match = "^- %[ %] TODO:", gsub = { "%- %[ %] TODO:", "- [x]" } },
	{ match = "^- %[ %]", gsub = { "%- %[ %]", "- [x]" } },
	{ match = "^- %[x%]", gsub = { "%- %[x%]", "-" }, undo_gsub = { "%- %[x%]", "- [ ]" } },
	{ match = "^- ", test_todo = true, gsub = { "%- ", "- [ ] TODO: " } },
}

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------
local function toggle_line(line, only_todo)
	local stripped = vim.trim(line)
	for _, t in ipairs(transitions) do
		if stripped:find(t.match) then
			if t.test_todo and stripped:find("TODO:") then
				return (line:gsub("%- ", "- [ ] ", 1))
			end
			if only_todo and t.undo_gsub then
				return (line:gsub(t.undo_gsub[1], t.undo_gsub[2], 1))
			end
			return (line:gsub(t.gsub[1], t.gsub[2], 1))
		end
	end
	return (line:gsub("(%S)", "- [ ] TODO: %1", 1))
end

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
function M.grep(opts)
	local merged = vim.tbl_deep_extend("force", default_grep_opts, opts or {})
	require("fzf-lua").grep(merged)
end

function M.toggle(opts)
	opts = opts or {}
	local cur = vim.api.nvim_win_get_cursor(0)[1]
	local startline, endline = cur, cur
	if opts.v then
		-- Read + clear the visual marks. Clearing matters: the keymap is
		-- bound in normal mode and always passes `v = true`, so a second
		-- press without a fresh visual selection would re-toggle the
		-- previous range. Clearing forces the next press without a new
		-- selection to fall through to the cursor line below.
		startline = vim.api.nvim_buf_get_mark(0, "<")[1]
		endline = vim.api.nvim_buf_get_mark(0, ">")[1]
		vim.api.nvim_buf_set_mark(0, "<", 0, 0, {})
		vim.api.nvim_buf_set_mark(0, ">", 0, 0, {})
		if startline <= 0 or endline <= 0 then
			startline, endline = cur, cur
		end
	end
	for lnum = startline, endline do
		local line = vim.api.nvim_buf_get_lines(0, lnum - 1, lnum, false)[1]
		vim.api.nvim_buf_set_lines(0, lnum - 1, lnum, false, { toggle_line(line, opts.onlyTodo) })
	end
end

return M
