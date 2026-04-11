----------------------------------------------------------------------
-- State
----------------------------------------------------------------------
local seen = {}

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------
local function modes_list(mode)
	return type(mode) == "table" and mode or { mode }
end

local function has_existing_global_map(mode, lhs)
	for _, existing in ipairs(vim.api.nvim_get_keymap(mode)) do
		if existing.lhs == lhs then
			return true
		end
	end
	return false
end

local function register_global(mode, lhs)
	for _, m in ipairs(modes_list(mode)) do
		local key = table.concat({ m, lhs }, "\0")
		if seen[key] then
			error(string.format("Duplicate keymap for mode=%s lhs=%s", m, lhs))
		end
		if has_existing_global_map(m, lhs) then
			error(string.format("Keymap already exists for mode=%s lhs=%s", m, lhs))
		end
		seen[key] = true
	end
end

local function map(mode, lhs, rhs, opts)
	if not (opts and opts.buffer) then
		register_global(mode, lhs)
	end
	vim.keymap.set(mode, lhs, rhs, opts)
end

----------------------------------------------------------------------
-- Keymap Modules
----------------------------------------------------------------------
require("keymaps.core")(map)
require("keymaps.lsp")(map)
require("keymaps.git")(map)
require("keymaps.find")(map)
require("keymaps.search")(map)
require("keymaps.notes")(map)
