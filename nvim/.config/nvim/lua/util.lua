----------------------------------------------------------------------
-- Module
----------------------------------------------------------------------
local M = {}

----------------------------------------------------------------------
-- Path Helpers
----------------------------------------------------------------------
function M.buf_dir(bufnr)
	bufnr = bufnr or 0
	local path = vim.api.nvim_buf_get_name(bufnr)
	local dir = path ~= "" and vim.fs.dirname(path) or nil
	return (dir and dir ~= "" and vim.uv.fs_stat(dir)) and dir or vim.fn.getcwd()
end

-- Nearest VCS root above the buffer, else the buffer's directory.
function M.vcs_dir(bufnr)
	return vim.fs.root(bufnr or 0, { ".git", ".jj", ".hg" }) or M.buf_dir(bufnr)
end

----------------------------------------------------------------------
-- Picker Helpers
----------------------------------------------------------------------
function M.fzf_with_cwd(picker, cwd_fn, opts)
	require("fzf-lua")[picker](vim.tbl_extend("force", { cwd = cwd_fn() }, opts or {}))
end

return M
