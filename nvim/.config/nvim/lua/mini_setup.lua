-- Mini.nvim module setup

local hipatterns = require("mini.hipatterns")
local indentscope = require("mini.indentscope")
local clue = require("mini.clue")
local notify = require("mini.notify")

-- Core Mini Modules
require("mini.icons").setup()
require("mini.surround").setup()
notify.setup({
	lsp_progress = { enable = false },
})
require("mini.bracketed").setup()
require("mini.cursorword").setup()

-- Module-Specific Configuration
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

-- Clue
-- Keep triggers and clues together so the whole setup can be audited in one
-- place. Only add prefixes you actually want mini.clue to intercept.
--
-- Keep the `<leader>` labels in sync with the sections in `lua/keymaps/`.
local clue_triggers = {
	{ mode = "n", keys = "<leader>" },
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
	{ mode = "n", keys = "<leader>s", desc = "+search" },
	{ mode = "n", keys = "<leader>c", desc = "+code" },
	{ mode = "n", keys = "<leader>e", desc = "+diagnostics" },
	{ mode = "n", keys = "<leader>u", desc = "+undo" },
	{ mode = "n", keys = "<leader>g", desc = "+git/vcs" },
}

clue.setup({
	triggers = clue_triggers,
	clues = clue_clues,
	window = { delay = 300 },
})

-- Native 0.12 `gx`/`gf` are callbacks with no `desc`, so mini.clue shows them
-- blank. Rewrite the mapping desc (what the clue window reads) on VimEnter,
-- since these mappings are created after this file loads.
vim.api.nvim_create_autocmd("VimEnter", {
	callback = function()
		for _, m in ipairs({
			{ mode = "n", lhs = "gx", desc = "Open URL/file under cursor" },
			{ mode = "n", lhs = "gf", desc = "Open file path under cursor" },
			{ mode = "n", lhs = "g%", desc = "Jump to prev matching pair (matchit)" },
		}) do
			pcall(clue.set_mapping_desc, m.mode, m.lhs, m.desc)
		end
	end,
})

require("mini.statusline").setup()
vim.opt.laststatus = 3

-- Route vim.notify through mini.notify
vim.notify = notify.make_notify()
