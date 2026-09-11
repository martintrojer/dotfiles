-- LSP server configs (0.12 vim.lsp.config)
-- See README.md for install instructions
local lua_globals = require("lua_globals")

----------------------------------------------------------------------
-- Server Configs
----------------------------------------------------------------------
vim.lsp.config("lua_ls", {
	cmd = { "lua-language-server" },
	filetypes = { "lua" },
	root_markers = { ".luarc.json", ".luarc.jsonc", ".stylua.toml", "stylua.toml" },
	on_attach = function(client)
		client.server_capabilities.documentFormattingProvider = false
		client.server_capabilities.documentRangeFormattingProvider = false
	end,
	settings = {
		Lua = {
			runtime = { version = "LuaJIT" },
			diagnostics = { globals = lua_globals.globals },
			workspace = { checkThirdParty = false, library = { vim.env.VIMRUNTIME } },
			telemetry = { enable = false },
		},
	},
})

vim.lsp.config("bashls", {
	cmd = { "bash-language-server", "start" },
	filetypes = { "sh", "bash", "zsh" },
	root_markers = { ".git" },
})

vim.lsp.config("html", {
	cmd = { "vscode-html-language-server", "--stdio" },
	filetypes = { "html" },
	root_markers = { ".git" },
})

vim.lsp.config("cssls", {
	cmd = { "vscode-css-language-server", "--stdio" },
	filetypes = { "css", "scss", "less" },
	root_markers = { ".git" },
})

vim.lsp.config("taplo", {
	cmd = { "taplo", "lsp", "stdio" },
	filetypes = { "toml" },
	root_markers = { "taplo.toml", ".taplo.toml", ".git" },
})

vim.lsp.config("ty", {
	cmd = { "ty", "server" },
	filetypes = { "python" },
	root_markers = { "pyproject.toml", "setup.py", "setup.cfg", "requirements.txt", ".git" },
	settings = {
		ty = {
			diagnosticMode = "workspace",
			completions = {
				autoImport = true,
			},
		},
	},
})

vim.lsp.config("ruff", {
	cmd = { "ruff", "server" },
	filetypes = { "python" },
	root_markers = { "pyproject.toml", "setup.py", "setup.cfg", "requirements.txt", ".git" },
	init_options = {
		settings = {
			showSyntaxErrors = false,
			fixAll = true,
			organizeImports = true,
		},
	},
})

vim.lsp.config("ts_ls", {
	cmd = { "typescript-language-server", "--stdio" },
	filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
	root_markers = { "tsconfig.json", "jsconfig.json", "package.json", ".git" },
})

vim.lsp.config("gopls", {
	cmd = { "gopls" },
	filetypes = { "go", "gomod", "gowork", "gotmpl" },
	root_markers = { "go.work", "go.mod", ".git" },
})

vim.lsp.config("rust_analyzer", {
	cmd = { "rust-analyzer" },
	filetypes = { "rust" },
	root_markers = { "Cargo.toml", "rust-project.json", ".git" },
})

vim.lsp.config("typos_lsp", {
	cmd = { "typos-lsp" },
	root_markers = { ".git" },
})

----------------------------------------------------------------------
-- Enabled Servers
----------------------------------------------------------------------
vim.lsp.enable({
	"lua_ls",
	"bashls",
	"html",
	"cssls",
	"taplo",
	"ty",
	"ruff",
	"ts_ls",
	"gopls",
	"rust_analyzer",
	"typos_lsp",
})
