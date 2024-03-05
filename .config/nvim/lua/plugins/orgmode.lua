return {
	"nvim-orgmode/orgmode",
	dependencies = {
		{ "nvim-treesitter/nvim-treesitter" },
	},
	ft = "org",
	lazy = true,
	config = function()
		-- Load treesitter grammar for org
		local orgmode = require("orgmode")
		-- Setup orgmode
		orgmode.setup({
			org_agenda_files = "~/docs/*.org",
			org_default_notes_file = "~/docs/Stuff.org",
		})
		orgmode.setup_ts_grammar()
	end,
}
