return { -- Bookmarking important files.
	"tomasky/bookmarks.nvim",
	config = function()
		local bookmarks = require("bookmarks")
		bookmarks.setup({

			save_file = vim.fn.expand("$HOME/.bookmarks"), -- bookmarks save file path
			keywords = {
				["@t"] = "☑️ ", -- mark annotation startswith @t ,signs this icon as `Todo`
				["@w"] = "⚠️ ", -- mark annotation startswith @w ,signs this icon as `Warn`
				["@f"] = "⛏ ", -- mark annotation startswith @f ,signs this icon as `Fix`
				["@n"] = " ", -- mark annotation startswith @n ,signs this icon as `Note`
			},
			on_attach = function()
				local bm = require("bookmarks")
				local map = vim.keymap.set
				map("n", "mm", bm.bookmark_toggle, { desc = "[b]ookmark [t]oggle" }) -- add or remove bookmark at current line
				map("n", "mi", bm.bookmark_ann, { desc = "[b]ookmark add/edit" }) -- add or edit mark annotation at current line
				map("n", "mc", bm.bookmark_clean, { desc = "[b]ookmark [c]lean (current file)" }) -- clean all marks in local buffer
				map("n", "mn", bm.bookmark_next, { desc = "[b]ookmark jump [n]ext" }) -- jump to next mark in local buffer
				map("n", "mp", bm.bookmark_prev, { desc = "[b]ookmark jump [p]rev" }) -- jump to previous mark in local buffer
				map("n", "ml", bm.bookmark_list, { desc = "[b]ookmark [l]ist" }) -- show marked file list in quickfix window
			end,
		})
	end,
}
