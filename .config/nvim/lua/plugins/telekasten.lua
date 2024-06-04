return {
	"martintrojer/telekasten.nvim",
	dependencies = {
		"telescope-symbols.nvim",
		"nvim-telescope/telescope.nvim",
		-- "renerocksai/calendar-vim",
		"dhruvasagar/vim-table-mode",
	},
	event = "VeryLazy",
	config = function()
		require("telekasten").setup({
			home = vim.fn.expand("~/notes"),
			smart_inbox_folder = vim.fn.expand("~/notes/inbox"),
			dailies = vim.fn.expand("~/notes/journal"),
			weeklies = vim.fn.expand("~/notes/journal"),
			templates = vim.fn.expand("~/notes/templates"),

			template_new_note = vim.fn.expand("~/notes/templates/note.md"),
			template_new_daily = vim.fn.expand("~/notes/templates/daily.md"),
			template_new_weekly = vim.fn.expand("~/notes/templates/weekly.md"),

			tag_notation = "yaml-bare",
			auto_set_filetype = false,
			filename_space_subst = "_",

			calendar_opts = {
				weeknm = 1,
			},
		})

		-- Launch panel if nothing is typed after <leader>z
		vim.keymap.set("n", "<leader>zp", "<cmd>Telekasten panel<CR>")

		-- Most used functions
		vim.keymap.set("n", "<leader>zf", "<cmd>Telekasten find_notes<CR>")
		vim.keymap.set("n", "<leader>zg", "<cmd>Telekasten search_notes<CR>")
		vim.keymap.set("n", "<leader>zd", "<cmd>Telekasten goto_today<CR>")
		vim.keymap.set("n", "<leader>zw", "<cmd>Telekasten goto_thisweek<CR>")
		vim.keymap.set("n", "<leader>zo", "<cmd>Telekasten follow_link<CR>")
		vim.keymap.set("n", "<leader>zn", "<cmd>Telekasten new_note<CR>")
		vim.keymap.set("n", "<leader>zb", "<cmd>Telekasten show_backlinks<CR>")
		vim.keymap.set("n", "<leader>zt", "<cmd>Telekasten toggle_todo<CR>")
		vim.keymap.set("n", "<leader>zz", "<cmd>Telekasten show_tags<CR>")

		vim.keymap.set("n", "<leader>zc", function()
			local pos = vim.api.nvim_win_get_cursor(0)[2]
			local line = vim.api.nvim_get_current_line()
			local nline = line:sub(0, pos) .. "PROMPT : RESPONSE 🧠 #tag" .. line:sub(pos + 1)
			vim.api.nvim_set_current_line(nline)
		end, { desc = "Insert flash card" })

		vim.keymap.set("n", "<leader>zi", function()
			local pos = vim.api.nvim_win_get_cursor(0)[2]
			local line = vim.api.nvim_get_current_line()
			local nline = line:sub(0, pos) .. "## " .. os.date("%Y-%m-%d") .. line:sub(pos + 1)
			vim.api.nvim_set_current_line(nline)
		end, { desc = "Insert current date" })
	end,
}
