-- Markdown filetype settings

----------------------------------------------------------------------
-- Preview clues
----------------------------------------------------------------------
-- Keep these labels in sync with the markdown-only `<leader>` mappings below.
vim.b.miniclue_config = vim.tbl_deep_extend("force", vim.b.miniclue_config or {}, {
	clues = {
		{ mode = "n", keys = "<leader>p", desc = "+preview" },
	},
})

----------------------------------------------------------------------
-- Markdown reading defaults
----------------------------------------------------------------------
-- Visually wrap long prose instead of requiring horizontal scrolling.
vim.opt_local.wrap = true
-- Prefer wrapping at word boundaries rather than mid-word.
vim.opt_local.linebreak = true
-- Keep wrapped continuation lines aligned with the original indent.
vim.opt_local.breakindent = true

----------------------------------------------------------------------
-- Buffer-Local Keymaps
----------------------------------------------------------------------
local util = require("util")

util.buf_map("n", "<leader>pp", function()
	require("nabla").popup()
end, { desc = "LaTeX popup" })

util.buf_map("n", "<leader>pr", function()
	require("markdown_read_mode").toggle()
end, { desc = "Toggle markdown read mode" })
