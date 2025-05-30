return { -- Collection of various small independent plugins/modules
	"echasnovski/mini.nvim",
	event = "VeryLazy",
	config = function()
		--  - va)  - [V]isually select [A]round [)]paren
		--  - yinq - [Y]ank [I]nside [N]ext [']quote
		--  - ci'  - [C]hange [I]nside [']quote
		require("mini.ai").setup({ n_lines = 500 })

		-- Add/delete/replace surroundings (brackets, quotes, etc.)
		-- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
		-- - sd'   - [S]urround [D]elete [']quotes
		-- - sr)'  - [S]urround [R]eplace [)] [']
		require("mini.surround").setup()

		-- Simple and easy statusline.
		local statusline = require("mini.statusline")
		statusline.setup()

		-- cursor location to LINE:COLUMN
		---@diagnostic disable-next-line: duplicate-set-field
		statusline.section_location = function()
			return "%2l:%-2v"
		end

		require("mini.sessions").setup()

		local trailspace = require("mini.trailspace")
		trailspace.setup()

		vim.api.nvim_create_autocmd("BufWritePre", {
			group = vim.api.nvim_create_augroup("trim-whitespace", { clear = true }),
			callback = function()
				trailspace.trim()
				trailspace.trim_last_lines()
			end,
		})
	end,
}
