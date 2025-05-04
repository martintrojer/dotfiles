return {
	"stevearc/conform.nvim",
	event = "VeryLazy",
	opts = {
		notify_on_error = false,
		format_on_save = {
			timeout_ms = 500,
			lsp_fallback = true,
		},
		formatters_by_ft = {
			css = { "prettier" },
			html = { "prettier" },
			javascript = { "prettier" },
			json = { "prettier" },
			jsonc = { "prettier" },
			lua = { "stylua" },
			python = { "ruff_format", "ruff_fix" },
			rust = { "rustfmt" },
		},
	},
}
