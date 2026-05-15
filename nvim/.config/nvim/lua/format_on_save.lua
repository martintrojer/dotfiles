-- Format on save: LSP -> CLI fallback by filetype -> trailspace trim
--
-- Why not conform.nvim? See nvim/README.md ("Philosophy"): 0.12
-- builtins first, mini.nvim is the plugin suite, every other plugin
-- has to earn its place. conform would mean a new plugin to wrap
-- shell-outs to formatters we already have on $PATH. The wrapper
-- below is ~30 lines, has no dependencies, and matches the
-- LSP-format autocmd pattern that was already here.

----------------------------------------------------------------------
-- Module
----------------------------------------------------------------------
local M = {}

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------
-- Run a CLI formatter, piping the buffer through stdin and replacing
-- the buffer's lines if the output differs. No-op if the executable
-- is missing or the formatter exits non-zero (logged as a warning).
local function run_cli(bufnr, cmd)
	if vim.fn.executable(cmd[1]) ~= 1 then
		return
	end
	local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
	local input = table.concat(lines, "\n")
	local out = vim.system(cmd, { stdin = input }):wait()
	if out.code ~= 0 then
		vim.notify(cmd[1] .. ": " .. (out.stderr or "failed"), vim.log.levels.WARN)
		return
	end
	local new_lines = vim.split(out.stdout or "", "\n", { plain = true })
	-- formatters typically emit a trailing newline -> trailing empty string; drop it.
	if new_lines[#new_lines] == "" then
		table.remove(new_lines)
	end
	if vim.deep_equal(lines, new_lines) then
		return
	end
	vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, new_lines)
end

----------------------------------------------------------------------
-- Formatters
----------------------------------------------------------------------
local function lsp_format(bufnr)
	for _, c in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
		if c:supports_method("textDocument/formatting") then
			vim.lsp.buf.format({ bufnr = bufnr, timeout_ms = 500 })
			return true
		end
	end
	return false
end

local function prettier_format(bufnr)
	run_cli(bufnr, { "prettier", "--stdin-filepath", vim.api.nvim_buf_get_name(bufnr) })
end

local function stylua_format(bufnr)
	run_cli(bufnr, { "stylua", "-" })
end

local function shfmt_format(bufnr)
	-- shfmt does not support zsh; the FILETYPE_FORMATTERS table gates by ft.
	run_cli(bufnr, { "shfmt", "-i", "2", "-ci" })
end

local function trailspace_format(_)
	MiniTrailspace.trim()
	MiniTrailspace.trim_last_lines()
end

----------------------------------------------------------------------
-- Filetype -> CLI formatter mapping
----------------------------------------------------------------------
-- Used only when no attached LSP advertises formatting capability.
-- Keep zsh out of shfmt — bash-only formatter would mangle zsh syntax.
local FILETYPE_FORMATTERS = {
	markdown = prettier_format,
	json = prettier_format,
	jsonc = prettier_format,
	json5 = prettier_format,
	yaml = prettier_format,
	html = prettier_format,
	css = prettier_format,
	scss = prettier_format,
	less = prettier_format,
	graphql = prettier_format,
	vue = prettier_format,
	lua = stylua_format,
	sh = shfmt_format,
	bash = shfmt_format,
}

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
function M.setup()
	local group = vim.api.nvim_create_augroup("format_on_save", { clear = true })
	vim.api.nvim_create_autocmd("BufWritePre", {
		group = group,
		callback = function(args)
			if not vim.bo[args.buf].modifiable then
				return
			end
			local formatted = lsp_format(args.buf)
			if not formatted then
				local fmt = FILETYPE_FORMATTERS[vim.bo[args.buf].filetype]
				if fmt then
					fmt(args.buf)
				end
			end
			trailspace_format(args.buf)
		end,
	})
end

return M
