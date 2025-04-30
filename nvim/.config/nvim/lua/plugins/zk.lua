return {
	"zk-org/zk-nvim",
	event = "VeryLazy",
	config = function()
		require("zk").setup({
			picker = "telescope",
			-- See Setup section below
		})

		vim.keymap.set("n", "<leader>zn", "<Cmd>ZkNew { title = vim.fn.input('Title: ') }<CR>", opts)
		vim.keymap.set("n", "<leader>zw", "<Cmd>ZkNew { group = 'journal' }<CR>", opts)
		vim.keymap.set("n", "<leader>zf", "<Cmd>ZkNotes { sort = { 'modified' } }<CR>", opts)
		vim.keymap.set("v", "<leader>zf", ":'<,'>ZkMatch<CR>", opts)
		vim.keymap.set("n", "<leader>zz", "<Cmd>ZkTags<CR>", opts)

		-- Search for the notes matching a given query.
		vim.keymap.set(
			"n",
			"<leader>zf",
			"<Cmd>ZkNotes { sort = { 'modified' }, match = { vim.fn.input('Search: ') } }<CR>",
			opts
		)

		vim.keymap.set("n", "<leader>zl", "<Cmd>ZkLinks<CR>", opts)
		vim.keymap.set("n", "<leader>zb", "<Cmd>ZkBacklinks<CR>", opts)
	end,
}
