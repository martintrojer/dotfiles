----------------------------------------------------------------------
-- Module
----------------------------------------------------------------------
local M = {}

----------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------
M.vcs_root_markers = { ".git", ".jj", ".hg" }

----------------------------------------------------------------------
-- Path Helpers
----------------------------------------------------------------------
function M.buf_dir(bufnr)
	bufnr = bufnr or 0
	local path = vim.api.nvim_buf_get_name(bufnr)
	local dir = path ~= "" and vim.fs.dirname(path) or nil
	return (dir and dir ~= "" and vim.uv.fs_stat(dir)) and dir or vim.fn.getcwd()
end

function M.vcs_root(bufnr)
	bufnr = bufnr or 0
	return vim.fs.root(bufnr, M.vcs_root_markers)
end

function M.vcs_dir(bufnr)
	return M.vcs_root(bufnr) or M.buf_dir(bufnr)
end

-- Notebook (zk vault) for the current buffer: nearest ancestor containing
-- a `.zk/` marker, falling back to `vim.g.notes_path`. Lets us drop into
-- any vault by `cd`-ing into it without re-pointing the default.
function M.notes_path(bufnr)
	return vim.fs.root(bufnr or 0, { ".zk" }) or vim.g.notes_path
end

----------------------------------------------------------------------
-- CWD Helpers
----------------------------------------------------------------------
function M.with_lcd(dir, fn)
	if not dir then
		return fn()
	end

	local win = vim.api.nvim_get_current_win()
	local prev_cwd = vim.fn.getcwd(win)

	vim.api.nvim_win_call(win, function()
		vim.cmd.lcd(dir)
	end)

	local ok, result = xpcall(fn, debug.traceback)

	if vim.api.nvim_win_is_valid(win) then
		vim.api.nvim_win_call(win, function()
			vim.cmd.lcd(prev_cwd)
		end)
	end

	if not ok then
		-- level=0 suppresses the file:line prefix Lua would otherwise prepend
		-- here; the xpcall traceback already carries the real location.
		error(result, 0)
	end
	return result
end

function M.with_vcs_root(fn, bufnr)
	local root = M.vcs_root(bufnr)
	if not root then
		vim.notify("No VCS root found for current buffer", vim.log.levels.WARN)
		return
	end
	return M.with_lcd(root, fn)
end

----------------------------------------------------------------------
-- Keymap Helpers
----------------------------------------------------------------------
function M.buf_map(mode, lhs, rhs, opts)
	vim.keymap.set(mode, lhs, rhs, vim.tbl_extend("force", { buffer = 0 }, opts or {}))
end

----------------------------------------------------------------------
-- Picker Helpers
----------------------------------------------------------------------
function M.fzf_with_cwd(picker, cwd_fn, opts)
	require("fzf-lua")[picker](vim.tbl_extend("force", { cwd = cwd_fn() }, opts or {}))
end

return M
