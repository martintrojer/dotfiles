-- Glow-like markdown read mode
--
-- Buffer-scoped: a markdown buffer is *marked* for read mode. While a
-- marked buffer is focused, the window gets reader-friendly options,
-- the global chrome (statusline) is hidden, the buffer is locked
-- read-only, diagnostics are muted, and render-markdown is enabled.
--
-- Because the mark lives on the buffer (not the window), switching
-- away to a source file and back works: focusing a non-marked buffer
-- restores normal chrome, focusing a marked buffer re-applies reader
-- chrome. Following a wikilink to another markdown buffer carries the
-- mark forward so the new buffer is also a reader.

----------------------------------------------------------------------
-- Module
----------------------------------------------------------------------
local M = {}

----------------------------------------------------------------------
-- Defaults
----------------------------------------------------------------------
local READER_WIN = {
	conceallevel = 3,
	concealcursor = "nvic",
	number = false,
	relativenumber = false,
	signcolumn = "no",
	list = false,
	cursorline = false,
	spell = false,
}

local SAVED_KEYS = {
	"conceallevel",
	"concealcursor",
	"number",
	"relativenumber",
	"signcolumn",
	"list",
	"cursorline",
	"spell",
}

-- Global options hidden while a marked buffer is focused.
local READER_GLOBAL = {
	laststatus = 0,
}
local saved_global = nil

local function global_on()
	if saved_global == nil then
		saved_global = {}
		for k, _ in pairs(READER_GLOBAL) do
			saved_global[k] = vim.o[k]
		end
		for k, v in pairs(READER_GLOBAL) do
			vim.o[k] = v
		end
	end
end

local function global_off()
	if saved_global ~= nil then
		for k, v in pairs(saved_global) do
			vim.o[k] = v
		end
		saved_global = nil
	end
end

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------
local function snapshot_win()
	local snap = {}
	for _, k in ipairs(SAVED_KEYS) do
		snap[k] = vim.api.nvim_get_option_value(k, { scope = "local", win = 0 })
	end
	return snap
end

local function apply_win(values)
	for k, v in pairs(values) do
		vim.api.nvim_set_option_value(k, v, { scope = "local", win = 0 })
	end
end

local function marked(buf)
	buf = buf or 0
	return vim.b[buf].read_mode == true
end

-- Apply reader chrome to the current window + buffer.
local function apply()
	if vim.w.read_mode_saved == nil then
		vim.w.read_mode_saved = snapshot_win()
	end
	apply_win(READER_WIN)
	global_on()
	vim.api.nvim_set_option_value("modifiable", false, { buf = 0 })
	vim.api.nvim_set_option_value("readonly", true, { buf = 0 })
	pcall(vim.diagnostic.enable, false, { bufnr = 0 })
	pcall(vim.cmd, "RenderMarkdown buf_enable")
end

-- Remove reader chrome from the current window + buffer.
local function restore()
	local saved = vim.w.read_mode_saved or READER_WIN
	apply_win(saved)
	vim.w.read_mode_saved = nil
	global_off()
	vim.api.nvim_set_option_value("modifiable", true, { buf = 0 })
	vim.api.nvim_set_option_value("readonly", false, { buf = 0 })
	pcall(vim.diagnostic.enable, true, { bufnr = 0 })
	pcall(vim.cmd, "RenderMarkdown buf_disable")
end

-- Reconcile the current window/buffer chrome with the focused
-- buffer's mark. Called on every buffer/window switch.
local function reconcile()
	-- Carry read mode into a freshly-focused markdown buffer when we
	-- arrived from another reader (link-following). The alternate
	-- buffer (`#`) is the one we came from, so no extra bookkeeping
	-- is needed.
	if not marked() and vim.bo.filetype == "markdown" then
		local alt = vim.fn.bufnr("#")
		if alt ~= -1 and marked(alt) then
			vim.b.read_mode = true
		end
	end
	if marked() then
		apply()
	else
		restore()
	end
end

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
function M.enable()
	vim.b.read_mode = true
	apply()
end

function M.disable()
	vim.b.read_mode = false
	restore()
end

function M.toggle()
	if marked() then
		M.disable()
	else
		M.enable()
	end
end

----------------------------------------------------------------------
-- Autocmds
----------------------------------------------------------------------
-- Reconcile chrome whenever the focused buffer/window changes. This
-- covers buffer switches among already-open buffers (BufEnter),
-- window focus changes (WinEnter), and first-time displays
-- (BufWinEnter, e.g. following a wikilink into a fresh buffer).
local group = vim.api.nvim_create_augroup("read_mode_follow", { clear = true })

vim.api.nvim_create_autocmd({ "BufEnter", "WinEnter", "BufWinEnter" }, {
	group = group,
	callback = reconcile,
})

return M
