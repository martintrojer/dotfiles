return {
	"williamboman/mason.nvim",
	dependencies = {
		"WhoIsSethDaniel/mason-tool-installer.nvim",
	},
	event = "VeryLazy",
	config = function()
		require("mason").setup()

		local ensure_installed = {
			"bash-language-server", -- bashls
			"black",
			"codelldb",
			"eslint-lsp", -- eslint
			"flake8",
			"gopls",
			"graphql-language-service-cli", -- graphql
			"haskell-language-server", -- hls
			"isort",
			"lua-language-server", -- lua_ls
			"luacheck",
			"marksman",
			"mypy",
			"prettier",
			"pyright",
			"ruff-lsp", -- ruff_lsp
			"rust-analyzer", -- rust_analyzer
			"stylua",
			"stylua",
			"typescript-language-server", -- ts_ls
			"typos-lsp", -- typos_lsp
		}
		require("mason-tool-installer").setup({ ensure_installed = ensure_installed })
	end,
}
