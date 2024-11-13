return {
	"olimorris/codecompanion.nvim",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-treesitter/nvim-treesitter",
		"hrsh7th/nvim-cmp",
		"nvim-telescope/telescope.nvim",
		{ "MeanderingProgrammer/render-markdown.nvim", ft = { "markdown", "codecompanion" } },
		{ "stevearc/dressing.nvim", opts = {} },
	},
	event = "VeryLazy",
	config = function()
		require("codecompanion").setup({
			-- strategies = {
			-- 	chat = {
			-- 		adapter = "qwen",
			-- 	},
			-- 	inline = {
			-- 		adapter = "qwen",
			-- 	},
			-- },
			adapters = {
				qwen = function()
					return require("codecompanion.adapters").extend("ollama", {
						name = "qwen", -- Give this adapter a different name to differentiate it from the default ollama adapter
						schema = {
							model = {
								default = "qwen2.5-coder:14b",
							},
							num_ctx = {
								default = 16384,
							},
							num_predict = {
								default = -1,
							},
						},
					})
				end,
			},
		})

		vim.api.nvim_set_keymap("n", "<C-a>", "<cmd>CodeCompanionActions<cr>", { noremap = true, silent = true })
		vim.api.nvim_set_keymap("v", "<C-a>", "<cmd>CodeCompanionActions<cr>", { noremap = true, silent = true })
		vim.api.nvim_set_keymap(
			"n",
			"<Leader>C",
			"<cmd>CodeCompanionChat Toggle<cr>",
			{ noremap = true, silent = true }
		)
		vim.api.nvim_set_keymap(
			"v",
			"<Leader>C",
			"<cmd>CodeCompanionChat Toggle<cr>",
			{ noremap = true, silent = true }
		)
		vim.api.nvim_set_keymap("v", "ga", "<cmd>CodeCompanionChat Add<cr>", { noremap = true, silent = true })

		-- Expand 'cc' into 'CodeCompanion' in the command line
		vim.cmd([[cab cc CodeCompanion]])
	end,
}
