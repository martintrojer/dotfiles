-- Copied from https://github.com/nvim-telekasten/telekasten.nvim/blob/6a10a7929421d6e696e46bbc5aa5627a8cbcf61d/lua/telekasten.lua
-- With some tweaks

local function ToggleTodo(opts)
	-- replace
	--       by -
	-- -     by - [ ] TODO:
	-- - [ ] by - [x]
	-- - [ ] TODO: by - [x]
	-- - [x] by -
	-- enter insert mode if opts.i == true
	-- if opts.v = true, then look for marks to toggle
	opts = opts or {}
	local startline = vim.api.nvim_buf_get_mark(0, "<")[1]
	local endline = vim.api.nvim_buf_get_mark(0, ">")[1]
	local cursorlinenr = vim.api.nvim_win_get_cursor(0)[1]
	-- to avoid the visual range marks not being reset when calling
	-- command from normal mode
	vim.api.nvim_buf_set_mark(0, "<", 0, 0, {})
	vim.api.nvim_buf_set_mark(0, ">", 0, 0, {})
	if startline <= 0 or endline <= 0 or opts.v ~= true then
		startline = cursorlinenr
		endline = cursorlinenr
	end
	for curlinenr = startline, endline do
		local curline = vim.api.nvim_buf_get_lines(0, curlinenr - 1, curlinenr, false)[1]
		local stripped = vim.trim(curline)
		local repline
		if vim.startswith(stripped, "- ") and not vim.startswith(stripped, "- [") then
			repline = curline:gsub("%- ", "- [ ] TODO: ", 1)
		else
			if vim.startswith(stripped, "- [ ] TODO:") then
				repline = curline:gsub("%- %[ %] TODO:", "- [x]", 1)
			elseif vim.startswith(stripped, "- [ ]") then
				repline = curline:gsub("%- %[ %]", "- [x]", 1)
			else
				if vim.startswith(stripped, "- [x]") then
					if opts.onlyTodo then
						repline = curline:gsub("%- %[x%]", "- [ ]", 1)
					else
						repline = curline:gsub("%- %[x%]", "-", 1)
					end
				else
					repline = curline:gsub("(%S)", "- [ ] %1", 1)
				end
			end
		end
		vim.api.nvim_buf_set_lines(0, curlinenr - 1, curlinenr, false, { repline })
		if opts.i then
			vim.api.nvim_feedkeys("A", "m", false)
		end
	end
end

vim.keymap.set("n", "<leader>zt", function()
	ToggleTodo({ v = true })
end, { desc = "Toggle todo" })
