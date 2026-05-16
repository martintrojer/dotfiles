-- Markdown filetype settings

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------
local util = require("util")
local vale_regex_hint =
	"# One regex per line. Use (?i) for case-insensitive matches and patterns like s? or (?:s|ies) for plurals, e.g. "

local function ensure_file(path, lines)
	if vim.fn.filereadable(path) == 1 then
		return
	end
	vim.fn.mkdir(vim.fs.dirname(path), "p")
	vim.fn.writefile(lines or {}, path)
end

local function ensure_vale_vocab(root)
	local vocab_name = "Local"
	local styles_dir = root .. "/.vale/styles"
	local vocab_dir = styles_dir .. "/config/vocabularies/" .. vocab_name
	local accept_path = vocab_dir .. "/accept.txt"
	local reject_path = vocab_dir .. "/reject.txt"
	local config_path = root .. "/.vale.ini"

	ensure_file(accept_path, {
		vale_regex_hint .. "(?i)mongodb",
	})
	ensure_file(reject_path, {
		vale_regex_hint .. "(?i)teh",
	})
	ensure_file(config_path, {
		"StylesPath = .vale/styles",
		"MinAlertLevel = suggestion",
		"Vocab = " .. vocab_name,
		"",
		"[*.md]",
		"BasedOnStyles = Vale",
	})

	return {
		accept = accept_path,
		reject = reject_path,
	}
end

local function edit_vale_vocab(kind)
	local root = vim.fs.root(0, util.vcs_root_markers)
	if not root then
		vim.notify("No project root found for Vale vocabulary", vim.log.levels.WARN)
		return
	end

	local vocab = ensure_vale_vocab(root)
	vim.cmd("edit " .. vim.fn.fnameescape(vocab[kind]))
end

-- Keep these labels in sync with the markdown-only `<leader>` mappings below.
vim.b.miniclue_config = vim.tbl_deep_extend("force", vim.b.miniclue_config or {}, {
	clues = {
		{ mode = "n", keys = "<leader>p", desc = "+preview" },
		{ mode = "n", keys = "<leader>t", desc = "+tools" },
	},
})

----------------------------------------------------------------------
-- Markdown editing defaults
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

-- LaTeX formula preview
util.buf_map("n", "<leader>pp", function()
	require("nabla").popup()
end, { desc = "LaTeX popup" })

-- Insert zk link
util.buf_map("i", "[[", "<Cmd>ZkInsertLink<CR>", { desc = "Insert zk link" })

-- Carddown card
util.buf_map("n", "<leader>tf", function()
	local pos = vim.api.nvim_win_get_cursor(0)[2]
	local line = vim.api.nvim_get_current_line()
	local nline = line:sub(0, pos) .. "PROMPT : RESPONSE 🧠 #tag" .. line:sub(pos + 1)
	vim.api.nvim_set_current_line(nline)
end, { desc = "Insert flash card" })

-- Date
util.buf_map("n", "<leader>td", function()
	local pos = vim.api.nvim_win_get_cursor(0)[2]
	local line = vim.api.nvim_get_current_line()
	local nline = line:sub(0, pos) .. "## " .. os.date("%Y-%m-%d") .. line:sub(pos + 1)
	vim.api.nvim_set_current_line(nline)
end, { desc = "Insert current date" })

----------------------------------------------------------------------
-- Note Helpers
----------------------------------------------------------------------
local todos = require("todos")

util.buf_map("n", "<leader>tt", function()
	todos.toggle({ v = true })
end, { desc = "Toggle todo" })

----------------------------------------------------------------------
-- Read mode (glow-like)
----------------------------------------------------------------------
util.buf_map("n", "<leader>pr", function()
	require("markdown_read_mode").toggle()
end, { desc = "Toggle markdown read mode" })

----------------------------------------------------------------------
-- Writing Tools
----------------------------------------------------------------------
local ts = require("timestamps")

util.buf_map("n", "<leader>tr", function()
	ts.reset()
end, { desc = "Reset timestamp timer and counter" })

util.buf_map("n", "<leader>tv", function()
	edit_vale_vocab("accept")
end, { desc = "Edit Vale accepted words" })

util.buf_map("n", "<leader>tV", function()
	edit_vale_vocab("reject")
end, { desc = "Edit Vale rejected words" })

util.buf_map("n", "<leader>tc", function()
	ts.disable_counter()
end, { desc = "Disable timestamp counter" })

util.buf_map("n", "<leader>ti", function()
	ts.get_ts_string(function(fullstr)
		local pos = vim.api.nvim_win_get_cursor(0)[2]
		local line = vim.api.nvim_get_current_line()
		local nline = line:sub(0, pos) .. "## " .. fullstr .. line:sub(pos + 1)
		vim.api.nvim_set_current_line(nline)
	end)
end, { desc = "Insert elapsed time (and counter) since timestamp start" })
