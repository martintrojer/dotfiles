-- :Sh — async shell with streaming output in a split
local buf

local function append(b, data)
	if not data then
		return
	end
	vim.schedule(function()
		if not vim.api.nvim_buf_is_valid(b) then
			return
		end
		local lines = vim.split(data, "\n", { trimempty = false })
		local last = vim.api.nvim_buf_line_count(b)
		local cur = vim.api.nvim_buf_get_lines(b, last - 1, last, false)[1] or ""
		lines[1] = cur .. lines[1]
		vim.api.nvim_buf_set_lines(b, last - 1, last, false, lines)
	end)
end

local function run_sh(args, autoclose)
	if not buf or not vim.api.nvim_buf_is_valid(buf) then
		buf = vim.api.nvim_create_buf(false, true)
		vim.bo[buf].buftype = "nofile"
		vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = buf })
	end

	-- Open split if not already visible, stay in current window.
	local visible = false
	for _, w in ipairs(vim.api.nvim_list_wins()) do
		if vim.api.nvim_win_get_buf(w) == buf then
			visible = true
			break
		end
	end
	if not visible then
		local prev = vim.api.nvim_get_current_win()
		vim.cmd("botright split")
		vim.api.nvim_win_set_buf(0, buf)
		vim.api.nvim_win_set_height(0, 15)
		vim.api.nvim_set_current_win(prev)
	end

	vim.bo[buf].modifiable = true
	vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "$ " .. args, "" })

	vim.system({ "sh", "-c", args }, {
		stdout = function(_, d)
			append(buf, d)
		end,
		stderr = function(_, d)
			append(buf, d)
		end,
	}, function(r)
		vim.schedule(function()
			if not vim.api.nvim_buf_is_valid(buf) then
				return
			end
			if r.code == 0 and autoclose then
				for _, w in ipairs(vim.api.nvim_list_wins()) do
					if vim.api.nvim_win_get_buf(w) == buf then
						vim.api.nvim_win_close(w, true)
					end
				end
			else
				vim.api.nvim_buf_set_lines(buf, -1, -1, false, { "exit " .. r.code })
				vim.bo[buf].modifiable = false
			end
		end)
	end)
end

vim.api.nvim_create_user_command("Sh", function(opts)
	run_sh(opts.args, true)
end, { nargs = "+", desc = "Run shell command async (auto-close on success)" })

vim.api.nvim_create_user_command("Shk", function(opts)
	run_sh(opts.args, false)
end, { nargs = "+", desc = "Run shell command async (keep output)" })
