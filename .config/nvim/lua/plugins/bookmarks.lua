return { -- Bookmarking important files.
	"tomasky/bookmarks.nvim",
	event = "VimEnter",
	config = function()
		local bm = require("bookmarks")
		local map = vim.keymap.set
		map("n", "<leader>mm", bm.bookmark_toggle, { desc = "[b]ookmark [t]oggle" }) -- add or remove bookmark at current line
		map("n", "<leader>mi", bm.bookmark_ann, { desc = "[b]ookmark add/edit" }) -- add or edit mark annotation at current line
		map("n", "<leader>mc", bm.bookmark_clean, { desc = "[b]ookmark [c]lean (current file)" }) -- clean all marks in local buffer
		map("n", "<leader>mn", bm.bookmark_next, { desc = "[b]ookmark jump [n]ext" }) -- jump to next mark in local buffer
		map("n", "<leader>mp", bm.bookmark_prev, { desc = "[b]ookmark jump [p]rev" }) -- jump to previous mark in local buffer
		map("n", "<leader>ml", bm.bookmark_list, { desc = "[b]ookmark [l]ist" }) -- show marked file list in quickfix window
		bm.setup({
			save_file = vim.fn.expand("$HOME/.bookmarks"), -- bookmarks save file path
		})
	end,
}
