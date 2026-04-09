-- Tab-based terminal helper
local M = {}

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

-- Open a command in a new tab terminal.
-- opts.title: tab name (shown in tabline)
-- opts.cwd:   working directory for the command
function M.open(cmd, opts)
	opts = opts or {}

	-- Check if the executable is available
	local bin = type(cmd) == "table" and cmd[1] or cmd:match("^%S+")
	if vim.fn.executable(bin) ~= 1 then
		vim.notify(bin .. " not found in PATH", vim.log.levels.WARN)
		return
	end

	-- Open new tab and set cwd if provided
	vim.cmd.tabnew()
	if opts.cwd then
		vim.cmd.lcd(opts.cwd)
	end

	-- Launch terminal, close tab on exit
	vim.cmd.terminal(cmd)
	local buf = vim.api.nvim_get_current_buf()
	if opts.title then
		vim.api.nvim_buf_set_name(buf, opts.title)
	end
	vim.bo[buf].buflisted = false

	vim.api.nvim_create_autocmd("TermClose", {
		buffer = buf,
		once = true,
		callback = function()
			vim.schedule(function()
				if vim.api.nvim_buf_is_valid(buf) then
					vim.api.nvim_buf_delete(buf, { force = true })
				end
			end)
		end,
	})

	vim.cmd("startinsert")
end

return M
