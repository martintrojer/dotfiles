-- Elapsed time headings for meeting/timed notes

----------------------------------------------------------------------
-- Module
----------------------------------------------------------------------
local M = {}

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
function M.reset()
	vim.g.timestamp_start = vim.fn.localtime()
	vim.g.timestamp_counter = 1
	vim.notify("timestamp and counter reset")
end

function M.disable_counter()
	vim.g.timestamp_counter = nil
	vim.notify("counter disabled")
end

function M.get_ts_string(callback)
	if not vim.g.timestamp_start then
		vim.g.timestamp_start = vim.fn.localtime()
		return
	end

	local elapsed = vim.fn.localtime() - vim.g.timestamp_start
	local timestr = elapsed < 60 and string.format("%ds", elapsed)
		or elapsed < 3600 and string.format("%dm", elapsed / 60)
		or string.format("%dh", elapsed / 3600)

	local counter = vim.g.timestamp_counter
	local prefix = counter and string.format("[%d] %s", counter, timestr) or timestr
	if counter then
		vim.g.timestamp_counter = counter + 1
	end

	vim.ui.input({ prompt = "Title: " }, function(title)
		callback(title and title ~= "" and (prefix .. " " .. title) or prefix)
	end)
end

return M
