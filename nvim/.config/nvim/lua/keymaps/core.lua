----------------------------------------------------------------------
-- Module References
----------------------------------------------------------------------
local history = require("history")

return function(map)
	----------------------------------------------------------------------
	-- General
	----------------------------------------------------------------------
	map("n", "<esc>", "<cmd>nohlsearch<cr>", { desc = "Clear search" })
	map("n", "ZX", "<cmd>update | bdelete<cr>", { desc = "Save and close buffer" })

	----------------------------------------------------------------------
	-- Diagnostics
	----------------------------------------------------------------------
	map("n", "<leader>ee", vim.diagnostic.open_float, { desc = "Diagnostic float" })
	map("n", "<leader>el", vim.diagnostic.setloclist, { desc = "Diagnostics to loclist" })
	map("n", "<leader>em", history.show_messages, { desc = "Messages history" })
	map("n", "<leader>en", history.show_notifications, { desc = "Notification history" })
	map("n", "<leader>u", "<cmd>Undotree<cr>", { desc = "Undotree" })

	----------------------------------------------------------------------
	-- Oil
	----------------------------------------------------------------------
	map("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })

	----------------------------------------------------------------------
	-- Terminal
	----------------------------------------------------------------------
	local term_buf = nil
	local term_win = nil
	local function toggle_terminal()
		if term_win and vim.api.nvim_win_is_valid(term_win) then
			vim.api.nvim_win_hide(term_win)
			term_win = nil
			return
		end
		if term_buf and vim.api.nvim_buf_is_valid(term_buf) then
			vim.cmd("botright split")
			term_win = vim.api.nvim_get_current_win()
			vim.api.nvim_win_set_buf(term_win, term_buf)
			vim.cmd("startinsert")
		else
			vim.cmd("botright split | term")
			term_buf = vim.api.nvim_get_current_buf()
			term_win = vim.api.nvim_get_current_win()
		end
		vim.api.nvim_win_set_height(term_win, 15)
	end

	map({ "n", "t" }, "<c-/>", toggle_terminal, { desc = "Toggle terminal" })
	map({ "n", "t" }, "<c-_>", toggle_terminal, { desc = "Toggle terminal" })
	map("t", "<esc><esc>", "<c-\\><c-n>", { desc = "Exit terminal mode" })

	----------------------------------------------------------------------
	-- Tmux Navigation
	----------------------------------------------------------------------
	local function tmux_terminal_map(lhs, command, desc)
		map("t", lhs, function()
			vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-\\><C-N>", true, false, true), "n", false)
			vim.schedule(function()
				vim.cmd(command)
			end)
		end, { desc = desc })
	end

	tmux_terminal_map("<c-h>", "TmuxNavigateLeft", "Tmux left")
	tmux_terminal_map("<c-j>", "TmuxNavigateDown", "Tmux down")
	tmux_terminal_map("<c-k>", "TmuxNavigateUp", "Tmux up")
	tmux_terminal_map("<c-l>", "TmuxNavigateRight", "Tmux right")

	map("n", "<c-h>", "<cmd>TmuxNavigateLeft<cr>", { desc = "Tmux left" })
	map("n", "<c-j>", "<cmd>TmuxNavigateDown<cr>", { desc = "Tmux down" })
	map("n", "<c-k>", "<cmd>TmuxNavigateUp<cr>", { desc = "Tmux up" })
	map("n", "<c-l>", "<cmd>TmuxNavigateRight<cr>", { desc = "Tmux right" })
end
