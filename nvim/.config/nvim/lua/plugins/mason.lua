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
			"codelldb",
			"eslint-lsp", -- eslint
			"gopls",
			"graphql-language-service-cli", -- graphql
			"haskell-language-server", -- hls
			"hlint",
			"jsonlint",
			"lua-language-server", -- lua_ls
			"luacheck",
			"markdownlint",
			"marksman",
			"prettier",
			"proselint",
			"pyright",
			"ruff",
			"rust-analyzer", -- rust_analyzer
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
