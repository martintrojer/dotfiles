function _G.ft_set_terminal_keymaps()
	local opts = { buffer = 0 }
	vim.keymap.set("t", "<esc>", [[<C-\><C-n>]], opts)
	vim.keymap.set("t", "<C-h>", [[<Cmd>wincmd h<CR>]], opts)
	vim.keymap.set("t", "<C-j>", [[<Cmd>wincmd j<CR>]], opts)
	vim.keymap.set("t", "<C-k>", [[<Cmd>wincmd k<CR>]], opts)
	vim.keymap.set("t", "<C-l>", [[<Cmd>wincmd l<CR>]], opts)
	vim.keymap.set("t", "<C-w>", [[<C-\><C-n><C-w>]], opts)
end

return {
	"voldikss/vim-floaterm",
	event = "VeryLazy",
	config = function()
		vim.g.floaterm_width = 0.8
		vim.g.floaterm_height = 0.8

		vim.keymap.set("n", "<F8>", "<cmd>FloatermNext<CR>")
		vim.keymap.set("t", "<F8>", "<C-\\><C-n><cmd>FloatermNext<CR>")
		vim.keymap.set("n", "<F9>", "<cmd>FloatermNew --wintype=vsplit --width=0.4<CR>")
		vim.keymap.set("n", "<F10>", "<cmd>FloatermNew<CR>")
		vim.keymap.set("n", "<F12>", "<cmd>FloatermToggle<CR>")
		vim.keymap.set("t", "<F12>", "<C-\\><C-n><cmd>FloatermToggle<CR>")

		vim.keymap.set("n", "<leader>o", "<cmd>FloatermNew ranger '%:h'<CR>", { desc = "Open Ranger" })
		vim.keymap.set(
			"n",
			"<leader>cr",
			"<cmd>FloatermNew carddown scan ~/notes --full; carddown revise<CR>",
			{ desc = "Carddown revise" }
		)

		vim.cmd("autocmd! TermOpen term://* lua ft_set_terminal_keymaps()")
	end,
}
