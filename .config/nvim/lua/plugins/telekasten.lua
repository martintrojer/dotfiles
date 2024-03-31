return {
	"renerocksai/telekasten.nvim",
	dependencies = {
		"telescope-symbols.nvim",
		"nvim-telescope/telescope.nvim",
		"renerocksai/calendar-vim",
		"dhruvasagar/vim-table-mode",
	},
	config = function()
		require("telekasten").setup({
			home = vim.fn.expand("~/notes"),
			dailies = vim.fn.expand("~/notes/journal"),
			weeklies = vim.fn.expand("~/notes/journal"),
			templates = vim.fn.expand("~/notes/templates"),

			template_new_note = vim.fn.expand("~/notes/templates/note.md"),
			template_new_daily = vim.fn.expand("~/notes/templates/daily.md"),
			template_new_weekly = vim.fn.expand("~/notes/templates/weekly.md"),

			tag_notation = "@tag",
			auto_set_filetype = false,

			calendar_opts = {
				weeknm = 1,
			},
		})

		-- Launch panel if nothing is typed after <leader>z
		vim.keymap.set("n", "<leader>z", "<cmd>Telekasten panel<CR>")

		-- Most used functions
		vim.keymap.set("n", "<leader>zf", "<cmd>Telekasten find_notes<CR>")
		vim.keymap.set("n", "<leader>zg", "<cmd>Telekasten search_notes<CR>")
		vim.keymap.set("n", "<leader>zd", "<cmd>Telekasten goto_today<CR>")
		vim.keymap.set("n", "<leader>zz", "<cmd>Telekasten follow_link<CR>")
		vim.keymap.set("n", "<leader>zn", "<cmd>Telekasten new_note<CR>")
		vim.keymap.set("n", "<leader>zc", "<cmd>Telekasten show_calendar<CR>")
		vim.keymap.set("n", "<leader>zb", "<cmd>Telekasten show_backlinks<CR>")
		vim.keymap.set("n", "<leader>zI", "<cmd>Telekasten insert_img_link<CR>")

		-- Call insert link automatically when we start typing a link
		vim.keymap.set("i", "[[", "<cmd>Telekasten insert_link<CR>")
	end,
}
