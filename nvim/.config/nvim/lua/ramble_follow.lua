-- Tell `ramble --follow` previews where the cursor is in a markdown buffer.
-- `ramble --goto FILE:LINE` does the rest (socket discovery, tmux window
-- scope); see ramble's docs/guide.md, "Following your editor".

if vim.fn.executable("ramble") == 0 then
	return
end

local last, timer

local function send()
	local path = vim.api.nvim_buf_get_name(0)
	if path == "" then
		return
	end
	local target = path .. ":" .. vim.api.nvim_win_get_cursor(0)[1]
	if target == last then
		return
	end
	last = target
	vim.system({ "ramble", "--goto", target })
end

vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "BufEnter" }, {
	group = vim.api.nvim_create_augroup("ramble_follow", {}),
	pattern = { "*.md", "*.markdown", "*.mdown", "*.mkd" },
	callback = function()
		timer = timer or vim.uv.new_timer()
		timer:stop()
		timer:start(50, 0, vim.schedule_wrap(send))
	end,
})
