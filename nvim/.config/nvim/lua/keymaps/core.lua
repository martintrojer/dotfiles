local map = vim.keymap.set

-- General
map("n", "<esc>", "<cmd>nohlsearch<cr>", { desc = "Clear search" })
map("n", "ZX", "<cmd>update | bdelete<cr>", { desc = "Save and close buffer" })

-- Diagnostics
map("n", "<leader>ee", vim.diagnostic.open_float, { desc = "Diagnostic float" })
map("n", "<leader>el", vim.diagnostic.setloclist, { desc = "Diagnostics to loclist" })
map("n", "<leader>u", "<cmd>Undotree<cr>", { desc = "Undotree" })

-- Oil
map("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })

-- Terminal
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

-- Pane Navigation: nvim splits, then the multiplexer at the edge. Inside tmux
-- that is vim-tmux-navigator; directly in a Rex pane it is `rex focus DIR`
-- (rex/.config/rex/init.lua passes Ctrl+h/j/k/l through to nvim).
local function navigate(wincmd, tmux_cmd, rex_dir)
	return function()
		if vim.env.TMUX then
			vim.cmd(tmux_cmd)
			return
		end
		local win = vim.api.nvim_get_current_win()
		vim.cmd("wincmd " .. wincmd)
		if vim.env.REX_SESSION and win == vim.api.nvim_get_current_win() then
			vim.system({ "rex", "focus", "-s", vim.env.REX_SESSION, rex_dir })
		end
	end
end

local function nav_map(lhs, wincmd, tmux_cmd, rex_dir, desc)
	local fn = navigate(wincmd, tmux_cmd, rex_dir)
	map("n", lhs, fn, { desc = desc })
	map("t", lhs, function()
		vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-\\><C-N>", true, false, true), "n", false)
		vim.schedule(fn)
	end, { desc = desc })
end

nav_map("<c-h>", "h", "TmuxNavigateLeft", "left", "Pane left")
nav_map("<c-j>", "j", "TmuxNavigateDown", "down", "Pane down")
nav_map("<c-k>", "k", "TmuxNavigateUp", "up", "Pane up")
nav_map("<c-l>", "l", "TmuxNavigateRight", "right", "Pane right")
