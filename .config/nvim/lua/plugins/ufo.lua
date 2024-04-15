return {
	"kevinhwang91/nvim-ufo",
	dependencies = {
		"kevinhwang91/promise-async",
	},

	config = function()
		vim.o.foldcolumn = "0"
		vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
		vim.o.foldlevelstart = 99
		vim.o.foldenable = true

		ufo = require("ufo")
		vim.keymap.set("n", "zR", ufo.openAllFolds)
		vim.keymap.set("n", "zM", ufo.closeAllFolds)

		ufo.setup({
			provider_selector = function(bufnr, filetype, buftype)
				return { "treesitter", "indent" }
			end,
		})

		-- remember position and folds
		local autosave_folds_group = vim.api.nvim_create_augroup("autosave-folds", { clear = true })
		vim.api.nvim_create_autocmd("BufWinLeave", {
			pattern = "*.*",
			group = autosave_folds_group,
			callback = function()
				vim.cmd.mkview()
			end,
		})
		vim.api.nvim_create_autocmd("BufWinEnter", {
			pattern = "*.*",
			group = autosave_folds_group,
			callback = function()
				vim.cmd.loadview({ mods = { emsg_silent = true } })
			end,
		})
	end,
}
