return { -- Magit
	"NeogitOrg/neogit",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"sindrets/diffview.nvim",
	},
	lazy = true,
	event = "VeryLazy",
	config = function()
		require("neogit").setup()
		vim.keymap.set("n", "<Leader>gs", ":Neogit cwd=%:p:h<CR>", { desc = "[G]it [S]tatus", noremap = true })
	end,
}
