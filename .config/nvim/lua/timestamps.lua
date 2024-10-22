local function set_start_time()
	vim.g.timestamp_start = vim.fn.localtime()
end

local function set_counter(val)
	vim.g.timestamp_counter = val
end

vim.keymap.set("n", "<leader>tz", function()
	set_start_time()
	set_counter(1)
	print("timestamp and counter reset")
end, { desc = "Reset timestamp timer and counter" })

vim.keymap.set("n", "<leader>tt", function()
	set_start_time()
	print("timestamp start reset")
end, { desc = "Reset timestamp to 'now'" })

vim.keymap.set("n", "<leader>tc", function()
	set_counter(1)
	print("counter reset")
end, { desc = "Reset timestamp counter" })

vim.keymap.set("n", "<leader>td", function()
	set_counter(nil)
	print("counter disabled")
end, { desc = "Disable timestamp counter" })

vim.keymap.set("n", "<leader>ti", function()
	if vim.g.timestamp_start == nil then
		set_start_time()
	end
	local elapsed = vim.fn.localtime() - vim.g.timestamp_start
	local counter = vim.g.timestamp_counter
	local counterstr = nil
	if counter then
		counterstr = string.format("[%d]", counter)
		vim.g.timestamp_counter = counter + 1
	end
	local timestr = nil
	if elapsed < 60 then
		timestr = string.format("%ds", elapsed)
	elseif elapsed < 60 * 60 then
		timestr = string.format("%dm", elapsed / 60)
	else
		timestr = string.format("%dh", elapsed / 60 / 60)
	end
	local fullstr = counterstr
	if fullstr then
		fullstr = fullstr .. " " .. timestr
	else
		fullstr = timestr
	end

	local pos = vim.api.nvim_win_get_cursor(0)[2]
	local line = vim.api.nvim_get_current_line()
	local nline = line:sub(0, pos) .. "## " .. fullstr .. line:sub(pos + 1)
	vim.api.nvim_set_current_line(nline)
end, { desc = "Insert elapsed time (and counter) since timestamp start" })
