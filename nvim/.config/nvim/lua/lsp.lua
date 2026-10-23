-- LSP server configs (0.12 vim.lsp.config)
-- See README.md for install instructions
local lua_globals = require("lua_globals")

-- Server Configs
vim.lsp.config("lua_ls", {
	cmd = { "lua-language-server" },
	filetypes = { "lua" },
	-- Nested list = equal priority; .git is the fallback so repos without a
	-- .luarc still get a workspace (and diagnostics).
	root_markers = { { ".luarc.json", ".luarc.jsonc", ".stylua.toml", "stylua.toml" }, ".git" },
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

-- TypeScript 7 (native Go port) ships its own LSP and no tsserver, so
-- typescript-language-server can't drive it.
vim.lsp.config("tsgo", {
	cmd = { "tsc", "--lsp", "--stdio" },
	filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
	root_markers = { "tsconfig.json", "jsconfig.json", "package.json", ".git" },
})

vim.lsp.config("gopls", {
	cmd = { "gopls" },
	filetypes = { "go", "gomod", "gowork" },
	root_markers = { "go.work", "go.mod", ".git" },
})

vim.lsp.config("rust_analyzer", {
	cmd = { "rust-analyzer" },
	filetypes = { "rust" },
	root_markers = { "Cargo.toml", "rust-project.json", ".git" },
})

-- mdroots finds note roots itself, so no root_markers; one client serves all.
vim.lsp.config("mdroots", {
	cmd = { "mdroots", "lsp" },
	filetypes = { "markdown", "org" },
	workspace_required = false,
	-- Complete as you type [[, ]( or # (no completion plugin in this config).
	on_attach = function(client, buf)
		vim.lsp.completion.enable(true, client.id, buf, { autotrigger = true })
	end,
})

vim.lsp.config("typos_lsp", {
	cmd = { "typos-lsp" },
	root_markers = { ".git" },
})

-- Enabled Servers
local servers = {
	"lua_ls",
	"bashls",
	"html",
	"cssls",
	"taplo",
	"ty",
	"ruff",
	"tsgo",
	"gopls",
	"rust_analyzer",
	"typos_lsp",
	"mdroots",
}
vim.lsp.enable(servers)

-- A missing server binary fails silently (the buffer just gets no LSP), so
-- name them once at startup. Install hints are in README.md.
vim.api.nvim_create_autocmd("VimEnter", {
	once = true,
	callback = function()
		local missing = {}
		for _, name in ipairs(servers) do
			local cmd = vim.lsp.config[name].cmd
			if type(cmd) == "table" and vim.fn.executable(cmd[1]) ~= 1 then
				table.insert(missing, cmd[1])
			end
		end
		if #missing > 0 then
			vim.notify("LSP servers not found: " .. table.concat(missing, ", "), vim.log.levels.WARN)
		end
	end,
})
