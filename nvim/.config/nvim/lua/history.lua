local M = {}

local function close_history_buffer(buf_id)
	local prev_buf = vim.b[buf_id].history_prev_buf
	if prev_buf and vim.api.nvim_buf_is_valid(prev_buf) then
		vim.api.nvim_win_set_buf(0, prev_buf)
		return
	end
	vim.cmd("bdelete")
end

local function prepare_history_buffer(buf_id, prev_buf)
	vim.b[buf_id].history_prev_buf = prev_buf
	vim.keymap.set("n", "q", function()
		close_history_buffer(buf_id)
	end, { buffer = buf_id, desc = "Close history" })
end

local function get_or_create_messages_buffer()
	-- Reuse a single scratch buffer so repeated opens do not create clutter.
	for _, id in ipairs(vim.api.nvim_list_bufs()) do
		if vim.api.nvim_buf_is_valid(id) and vim.bo[id].filetype == "messages-history" then
			return id
		end
	end

	local buf_id = vim.api.nvim_create_buf(true, true)
	vim.bo[buf_id].bufhidden = "wipe"
	vim.bo[buf_id].filetype = "messages-history"
	vim.api.nvim_buf_set_name(buf_id, "messages-history")
	return buf_id
end

function M.show_messages()
	-- `:messages` includes editor output that never went through `vim.notify()`.
	local lines = vim.split(vim.api.nvim_exec2("messages", { output = true }).output, "\n", { plain = true })
	if #lines > 0 and lines[#lines] == "" then
		table.remove(lines)
	end
	if vim.tbl_isempty(lines) then
		lines = { "No messages" }
	end

	local buf_id = get_or_create_messages_buffer()
	vim.bo[buf_id].modifiable = true
	vim.api.nvim_buf_set_lines(buf_id, 0, -1, false, lines)
	vim.bo[buf_id].modifiable = false
	prepare_history_buffer(buf_id, vim.api.nvim_get_current_buf())
	vim.api.nvim_win_set_buf(0, buf_id)
end

function M.show_notifications()
	-- mini.notify keeps its own separate history buffer.
	local prev_buf = vim.api.nvim_get_current_buf()
	require("mini.notify").show_history()
	prepare_history_buffer(vim.api.nvim_get_current_buf(), prev_buf)
end

return M
