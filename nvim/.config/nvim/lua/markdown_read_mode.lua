-- Glow-like markdown read mode
--
-- Window-scoped: enabling it sets reader-friendly window options and
-- locks the buffer read-only. Following a wikilink/markdown link in
-- the same window keeps read mode active and re-applies it to the new
-- buffer (so the new buffer is also locked, not silently editable).
--
-- Toggling off restores the saved window state and unlocks the buffer.

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
	wrap = true,
	linebreak = true,
	breakindent = true,
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
	"wrap",
	"linebreak",
	"breakindent",
}

-- Global options we hide while ANY window is in read mode. Tabline is
-- intentionally left alone. We refcount so opening multiple readers
-- and closing them in any order restores cleanly.
local READER_GLOBAL = {
	laststatus = 0,
}
local saved_global = nil
local active_count = 0

local function global_on()
	if active_count == 0 then
		saved_global = {}
		for k, _ in pairs(READER_GLOBAL) do
			saved_global[k] = vim.o[k]
		end
		for k, v in pairs(READER_GLOBAL) do
			vim.o[k] = v
		end
	end
	active_count = active_count + 1
end

local function global_off()
	active_count = math.max(0, active_count - 1)
	if active_count == 0 and saved_global then
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
		snap[k] = vim.wo[k]
	end
	return snap
end

local function apply_win(values)
	for k, v in pairs(values) do
		vim.wo[k] = v
	end
end

local function lock_buf()
	vim.bo.modifiable = false
	vim.bo.readonly = true
end

local function unlock_buf()
	vim.bo.modifiable = true
	vim.bo.readonly = false
end

local function enable_render()
	pcall(vim.cmd, "RenderMarkdown buf_enable")
end

local function disable_render()
	pcall(vim.cmd, "RenderMarkdown buf_disable")
end

-- Silence LSP/linter red squiggles in the current buffer. Spell
-- squiggles are already handled via `spell = false` in READER_WIN.
local function mute_diagnostics()
	pcall(vim.diagnostic.enable, false, { bufnr = 0 })
end

local function unmute_diagnostics()
	pcall(vim.diagnostic.enable, true, { bufnr = 0 })
end

-- Re-apply read mode to the current buffer/window. Called both on
-- initial toggle and from BufWinEnter when crossing into a new
-- buffer while read mode is active in this window.
local function apply()
	apply_win(READER_WIN)
	lock_buf()
	mute_diagnostics()
	enable_render()
end

local function on()
	return vim.w.read_mode == true
end

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
function M.enable()
	if on() then
		return
	end
	vim.w.read_mode_saved = snapshot_win()
	vim.w.read_mode = true
	global_on()
	apply()
end

function M.disable()
	if not on() then
		return
	end
	local saved = vim.w.read_mode_saved or READER_WIN
	apply_win(saved)
	unlock_buf()
	unmute_diagnostics()
	disable_render()
	vim.w.read_mode = false
	vim.w.read_mode_saved = nil
	global_off()
end

function M.toggle()
	if on() then
		M.disable()
	else
		M.enable()
	end
end

----------------------------------------------------------------------
-- Autocmds
----------------------------------------------------------------------
-- When read mode is active in a window and the user enters a new
-- buffer there (e.g. by following a link), re-apply read mode so the
-- new buffer is also locked + rendered. Filetype-gated so we don't
-- lock unrelated buffers (terminals, oil, etc.).
local group = vim.api.nvim_create_augroup("read_mode_follow", { clear = true })
vim.api.nvim_create_autocmd("BufWinEnter", {
	group = group,
	callback = function(args)
		if not on() then
			return
		end
		if vim.bo[args.buf].filetype ~= "markdown" then
			return
		end
		apply()
	end,
})

return M
