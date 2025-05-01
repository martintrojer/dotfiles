return {
	"zk-org/zk-nvim",
	event = "VeryLazy",
	config = function()
		require("zk").setup({
			picker = "telescope",
			-- See Setup section below
		})

		vim.keymap.set("n", "<leader>zn", "<Cmd>ZkNew { title = vim.fn.input('Title: ') }<CR>", { desc = "New Note" })
		vim.keymap.set(
			"n",
			"<leader>zw",
			"<Cmd>ZkNew { group = 'journal' }<CR>",
			{ desc = "Create/Edit Weekly Journal" }
		)
		vim.keymap.set("n", "<leader>zf", "<Cmd>ZkNotes { sort = { 'modified' } }<CR>", { desc = "Find Notes" })
		vim.keymap.set("v", "<leader>zf", ":'<,'>ZkMatch<CR>", { desc = "Find Notes Matching Selection" })
		vim.keymap.set("n", "<leader>zz", "<Cmd>ZkTags<CR>", { desc = "Find Notes by Tag" })

		-- Search for the notes matching a given query.
		vim.keymap.set(
			"n",
			"<leader>zs",
			"<Cmd>ZkNotes { sort = { 'modified' }, match = { vim.fn.input('Search: ') } }<CR>",
			{ desc = "Search Notes Matching Regex" }
		)

		vim.keymap.set("n", "<leader>zl", "<Cmd>ZkLinks<CR>", { desc = "Notes Linked By Current Note" })
		vim.keymap.set("n", "<leader>zb", "<Cmd>ZkBacklinks<CR>", { desc = "Notes Linking To Current Note" })
	end,
}
