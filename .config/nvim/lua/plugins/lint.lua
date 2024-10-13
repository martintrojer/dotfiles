return {
	"mfussenegger/nvim-lint",
	events = { "BufReadPre", "BufNewfile" },
	config = function()
		local lint = require("lint")
		lint.linters_by_ft = {
			lua = { "luacheck" },
			python = { "mypy", "flake8" },
			javascript = { "eslint" },
		}

		local lint_ag = vim.api.nvim_create_augroup("lint", { clear = true })
		vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "InsertLeave" }, {
			group = lint_ag,
			callback = function()
				lint.try_lint()
			end,
		})
		vim.keymap.set("n", "<leader>l", function()
			lint.try_lint()
		end, { desc = "Lint of current file" })
	end,
}
