-- Mini.nvim module setup

----------------------------------------------------------------------
-- Module References
----------------------------------------------------------------------
local hipatterns = require("mini.hipatterns")
local indentscope = require("mini.indentscope")
local clue = require("mini.clue")
local notify = require("mini.notify")
local palette = require("catppuccin.palettes").get_palette()

----------------------------------------------------------------------
-- Highlight Helpers
----------------------------------------------------------------------
local function set_hl(group, spec)
	vim.api.nvim_set_hl(0, group, spec)
end

local function apply_highlights(definitions)
	for group, spec in pairs(definitions) do
		set_hl(group, spec)
	end
end

local function style_cursorword()
	apply_highlights({
		MiniCursorword = { bg = palette.surface1, underline = false },
		MiniCursorwordCurrent = { bg = palette.surface1, underline = false },
	})
end

local function style_diagnostics()
	apply_highlights({
		DiagnosticUnderlineError = { sp = palette.red, undercurl = true, underline = false },
		DiagnosticUnderlineWarn = { sp = palette.yellow, undercurl = true, underline = false },
		DiagnosticUnderlineInfo = { sp = palette.blue, undercurl = true, underline = false },
		DiagnosticUnderlineHint = { sp = palette.teal, undercurl = true, underline = false },
		DiagnosticUnderlineOk = { sp = palette.green, undercurl = true, underline = false },
	})
end

local function style_diff()
	apply_highlights({
		MiniDiffSignAdd = { fg = palette.teal, bg = "NONE" },
		MiniDiffSignChange = { fg = palette.sapphire, bg = "NONE" },
		MiniDiffSignDelete = { fg = palette.maroon, bg = "NONE" },
	})
end

local function style_hipatterns()
	apply_highlights({
		MiniHipatternsTodo = { fg = palette.base, bg = palette.yellow, bold = true },
		MiniHipatternsFixme = { fg = palette.base, bg = palette.overlay1, bold = true },
		MiniHipatternsHack = { fg = palette.base, bg = palette.overlay0, bold = true },
		MiniHipatternsNote = { fg = palette.base, bg = palette.overlay0, bold = true },
	})
end

local function style_indentscope()
	set_hl("MiniIndentscopeSymbol", { fg = palette.surface1 })
end

local function style_tabline()
	apply_highlights({
		MiniTablineFill = { fg = palette.overlay0, bg = palette.mantle },
		MiniTablineCurrent = { fg = palette.base, bg = palette.blue, bold = true },
		MiniTablineVisible = { fg = palette.text, bg = palette.surface0, bold = true },
		MiniTablineHidden = { fg = palette.overlay1, bg = palette.mantle },
		MiniTablineModifiedCurrent = { fg = palette.base, bg = palette.peach, bold = true },
		MiniTablineModifiedVisible = { fg = palette.base, bg = palette.teal, bold = true },
		MiniTablineModifiedHidden = { fg = palette.peach, bg = palette.mantle, bold = true },
		MiniTablineTabpagesection = { fg = palette.crust, bg = palette.lavender, bold = true },
	})
end

local function apply_theme_overrides()
	style_cursorword()
	style_diagnostics()
	style_diff()
	style_hipatterns()
	style_indentscope()
	style_tabline()
end

----------------------------------------------------------------------
-- Component Helpers
----------------------------------------------------------------------
local function statusline_content()
	local mode, mode_hl = MiniStatusline.section_mode({ trunc_width = 120 })
	local git = MiniStatusline.section_git({ trunc_width = 75 })
	local diff = MiniStatusline.section_diff({ trunc_width = 75 })
	local diagnostics = MiniStatusline.section_diagnostics({ trunc_width = 75 })
	local filename = MiniStatusline.section_filename({ trunc_width = 140 })
	local lsp = MiniStatusline.section_lsp({ trunc_width = 75 })
	local fileinfo = MiniStatusline.section_fileinfo({ trunc_width = 120 })
	local location = MiniStatusline.section_location({ trunc_width = 75 })
	local search = MiniStatusline.section_searchcount({ trunc_width = 75 })

	return MiniStatusline.combine_groups({
		{ hl = mode_hl, strings = { mode } },
		{ hl = "MiniStatuslineDevinfo", strings = { git, diff, diagnostics } },
		"%<",
		{ hl = "MiniStatuslineFilename", strings = { filename } },
		"%=",
		{ hl = "MiniStatuslineDevinfo", strings = { lsp } },
		{ hl = "MiniStatuslineFileinfo", strings = { fileinfo } },
		{ hl = mode_hl, strings = { search, location } },
	})
end

----------------------------------------------------------------------
-- Core Mini Modules
----------------------------------------------------------------------
require("mini.icons").setup()
require("mini.surround").setup()
require("mini.git").setup()
notify.setup({
	lsp_progress = { enable = false },
})
require("mini.diff").setup({
	view = {
		style = "sign",
	},
})
require("mini.bracketed").setup()
require("mini.splitjoin").setup()
require("mini.move").setup()
require("mini.pairs").setup()
require("mini.cursorword").setup()

----------------------------------------------------------------------
-- Module-Specific Configuration
----------------------------------------------------------------------
hipatterns.setup({
	highlighters = {
		todo = { pattern = "%f[%w]()TODO()%f[%W]:", group = "MiniHipatternsTodo" },
		fix = { pattern = "%f[%w]()FIX()%f[%W]:", group = "MiniHipatternsFixme" },
		fixme = { pattern = "%f[%w]()FIXME()%f[%W]:", group = "MiniHipatternsFixme" },
		hack = { pattern = "%f[%w]()HACK()%f[%W]:", group = "MiniHipatternsHack" },
		note = { pattern = "%f[%w]()NOTE()%f[%W]:", group = "MiniHipatternsNote" },
		hex_color = hipatterns.gen_highlighter.hex_color(),
	},
})

indentscope.setup({
	draw = { animation = indentscope.gen_animation.none() },
})
require("mini.trailspace").setup()
require("mini.tabline").setup({
	tabpage_section = "right",
})

----------------------------------------------------------------------
-- mini.clue
----------------------------------------------------------------------
-- Keep triggers and clues together so the whole setup can be audited in one
-- place. Only add prefixes you actually want mini.clue to intercept.
--
-- Keep the `<leader>` labels in sync with the actual sections in `keymaps.lua`.
local clue_triggers = {
	{ mode = "n", keys = "<leader>" },
	{ mode = "x", keys = "<leader>" },
	{ mode = "n", keys = "g" },
	{ mode = "x", keys = "g" },
	{ mode = "n", keys = "'" },
	{ mode = "x", keys = "'" },
	{ mode = "n", keys = "`" },
	{ mode = "x", keys = "`" },
	{ mode = "n", keys = '"' },
	{ mode = "x", keys = '"' },
	{ mode = "n", keys = "z" },
	{ mode = "x", keys = "z" },
	{ mode = "n", keys = "<C-w>" },
	{ mode = "n", keys = "[" },
	{ mode = "n", keys = "]" },
	{ mode = "i", keys = "<C-x>" },
	{ mode = "i", keys = "<C-r>" },
	{ mode = "c", keys = "<C-r>" },
}

local clue_clues = {
	clue.gen_clues.square_brackets(),
	clue.gen_clues.builtin_completion(),
	clue.gen_clues.g(),
	clue.gen_clues.marks(),
	clue.gen_clues.registers(),
	clue.gen_clues.windows(),
	clue.gen_clues.z(),
	{ mode = "n", keys = "<leader>f", desc = "+find" },
	{ mode = "x", keys = "<leader>f", desc = "+find" },
	{ mode = "n", keys = "<leader>z", desc = "+notes" },
	{ mode = "x", keys = "<leader>z", desc = "+notes" },
	{ mode = "n", keys = "<leader>c", desc = "+code" },
	{ mode = "x", keys = "<leader>c", desc = "+code" },
	{ mode = "n", keys = "<leader>e", desc = "+diagnostics" },
	{ mode = "x", keys = "<leader>e", desc = "+diagnostics" },
	{ mode = "n", keys = "<leader>u", desc = "+undo" },
	{ mode = "x", keys = "<leader>u", desc = "+undo" },
}

clue.setup({
	triggers = clue_triggers,
	clues = clue_clues,
	window = { delay = 300 },
})

require("mini.statusline").setup({
	content = {
		active = statusline_content,
	},
})
vim.opt.laststatus = 3

----------------------------------------------------------------------
-- Theme Overrides
----------------------------------------------------------------------
apply_theme_overrides()

vim.api.nvim_create_autocmd("ColorScheme", {
	pattern = "*",
	callback = apply_theme_overrides,
})

-- Route vim.notify through mini.notify
vim.notify = notify.make_notify()
