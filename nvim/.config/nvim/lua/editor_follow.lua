-- Tell following debriefs and rambles where the cursor is. `--goto
-- FILE:LINE` does the rest (socket discovery, tmux window scope); one call
-- reaches both tools. debrief follows any file in the open commit, ramble
-- drops what isn't markdown. See "Following your editor" in either guide.

local bin = vim.fn.executable("debrief") == 1 and "debrief" or vim.fn.executable("ramble") == 1 and "ramble"
if not bin then
	return
end

local last, timer

local function send()
	if vim.bo.buftype ~= "" then
		return
	end
	local path = vim.api.nvim_buf_get_name(0)
	if path == "" then
		return
	end
	local target = path .. ":" .. vim.api.nvim_win_get_cursor(0)[1]
	if target == last then
		return
	end
	last = target
	vim.system({ bin, "--goto", target })
end

vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "BufEnter" }, {
	group = vim.api.nvim_create_augroup("editor_follow", {}),
	callback = function()
		timer = timer or vim.uv.new_timer()
		timer:stop()
		timer:start(50, 0, vim.schedule_wrap(send))
	end,
})
