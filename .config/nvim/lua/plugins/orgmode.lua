return {
	"nvim-orgmode/orgmode",
	dependencies = {
		{ "nvim-treesitter/nvim-treesitter", lazy = true },
		{ "lyz-code/telescope-orgmode.nvim" },
	},
	event = "VeryLazy",
	config = function()
		-- Load treesitter grammar for org
		local orgmode = require("orgmode")
		orgmode.setup_ts_grammar()

		-- Setup treesitter
		require("nvim-treesitter.configs").setup({
			highlight = {
				enable = true,
			},
			ensure_installed = { "org" },
		})

		-- Setup orgmode
		orgmode.setup({
			org_agenda_files = "~/notes/**/*",
			org_default_notes_file = "~/notes/stuff.org",
		})

		require("telescope").load_extension("orgmode")
		vim.keymap.set(
			"n",
			"<leader>so",
			require("telescope").extensions.orgmode.search_headings,
			{ desc = "Search [O]rg headers" }
		)
		vim.keymap.set(
			"n",
			"<leader>oR",
			require("telescope").extensions.orgmode.refile_heading,
			{ desc = "Refile heading" }
		)
	end,
}
