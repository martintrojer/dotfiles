----------------------------------------------------------------------
-- Module References
----------------------------------------------------------------------
local M = {}
local resolve_filename = require("fugitive-core.views.annotate").resolve_filename

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------
local function warn(msg)
	vim.notify(msg, vim.log.levels.WARN)
end

local function buffer_content(bufnr)
	return table.concat(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false), "\n")
end

local function index_content(root, relpath)
	local result = vim.system({ "git", "show", ":" .. relpath }, {
		cwd = root,
		text = true,
	}):wait()
	if result.code ~= 0 then
		return nil
	end

	return result.stdout
end

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
function M.current_file_side_by_side()
	local bufnr = vim.api.nvim_get_current_buf()
	local data = MiniGit.get_buf_data(bufnr)
	if not data or not data.root then
		warn("Current buffer is not tracked by mini.git")
		return
	end

	local relpath = resolve_filename(nil, data.root)
	if not relpath then
		warn("Current buffer is not a file")
		return
	end

	local left = index_content(data.root, relpath)
	if left == nil then
		warn(("No index version for %s"):format(relpath))
		return
	end

	require("fugitive-core.ui").open_sidebyside(
		left,
		relpath .. " (index)",
		buffer_content(bufnr),
		relpath .. " (buffer)",
		vim.api.nvim_buf_get_name(bufnr),
		{ repo_root = data.root }
	)
end

return M
