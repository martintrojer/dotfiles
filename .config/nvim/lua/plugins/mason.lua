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
			"hlint",
			"isort",
			"jsonlint",
			"lua-language-server", -- lua_ls
			"luacheck",
			"markdownlint",
			"marksman",
			"mypy",
			"prettier",
			"proselint",
			"pyright",
			"ruff",
			"rust-analyzer", -- rust_analyzer
			"rustfmt",
			"shellcheck",
			"sqlfluff",
			"stylua",
			"typescript-language-server", -- ts_ls
			"typos-lsp", -- typos_lsp
			"vale",
			"write-good",
		}
		require("mason-tool-installer").setup({ ensure_installed = ensure_installed })
	end,
}
