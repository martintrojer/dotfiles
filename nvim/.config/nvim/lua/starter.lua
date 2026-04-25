-- Start screen (mini.starter)
local starter = require("mini.starter")

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------
local cwd_glyph = " 󰉋"

local function action_item(section, name, action)
	return { name = name, action = action, section = section }
end

local function cwd_recent_files(n)
	local section = starter.sections.recent_files(n, true, false)
	return function()
		local items = section()
		for _, item in ipairs(items) do
			item.section = "Recent files" .. cwd_glyph
		end
		return items
	end
end

local function notes_section()
	local notes_path = vim.g.notes_path
	local home = vim.fn.expand("~")
	if notes_path == home then
		notes_path = "~"
	elseif notes_path:sub(1, #home + 1) == home .. "/" then
		notes_path = "~/" .. notes_path:sub(#home + 2)
	end
	return string.format("Notes (%s)", notes_path)
end

local function starter_header()
	local hour = tonumber(os.date("!%H")) -- UTC (UK)
	local greeting
	if hour < 6 then
		greeting = "🌙 Good night"
	elseif hour < 12 then
		greeting = "☀️ Good morning"
	elseif hour < 18 then
		greeting = "🌤️ Good afternoon"
	else
		greeting = "🌆 Good evening"
	end

	local logo = {
		[[  ███╗   ██╗██╗   ██╗██╗███╗   ███╗]],
		[[  ████╗  ██║██║   ██║██║████╗ ████║]],
		[[  ██╔██╗ ██║██║   ██║██║██╔████╔██║]],
		[[  ██║╚██╗██║╚██╗ ██╔╝██║██║╚██╔╝██║]],
		[[  ██║ ╚████║ ╚████╔╝ ██║██║ ╚═╝ ██║]],
		[[  ╚═╝  ╚═══╝  ╚═══╝  ╚═╝╚═╝     ╚═╝]],
		"",
		string.rep(" ", math.floor((36 - #greeting) / 2)) .. greeting,
	}

	return table.concat(logo, "\n")
end

----------------------------------------------------------------------
-- Starter Configuration
----------------------------------------------------------------------
local notes_header = notes_section()

starter.setup({
	items = {
		cwd_recent_files(5),
		action_item("Work", "Old files", function()
			require("fzf-lua").oldfiles()
		end),
		action_item("Work", "Find files" .. cwd_glyph, function()
			require("fzf-lua").files({ cwd = vim.uv.cwd() })
		end),
		action_item("Work", "Live grep" .. cwd_glyph, function()
			require("fzf-lua").live_grep({ cwd = vim.uv.cwd() })
		end),
		action_item("Work", "Semantic search", "VecgrepLive"),
		action_item(notes_header, "Notes", function()
			require("zk.commands").get("ZkNotes")({ notebook_path = vim.g.notes_path, sort = { "modified" } })
		end),
		action_item(notes_header, "Journal", function()
			require("zk.commands").get("ZkNew")({ notebook_path = vim.g.notes_path, group = "journal" })
		end),
		action_item(notes_header, "TODOs", function()
			require("todos").grep({ cwd = vim.g.notes_path })
		end),
		action_item("System", "Oil" .. cwd_glyph, function()
			require("oil").open(vim.uv.cwd())
		end),
		action_item("System", "README", function()
			vim.cmd("edit " .. vim.fn.expand("~/dotfiles/nvim/README.md"))
		end),
		action_item("System", "Update plugins and TS", function()
			vim.cmd("TSSync")
			vim.cmd("TSUpdate")
			vim.cmd("PackUpdate")
		end),
	},
	header = starter_header(),
	footer = "",
})

----------------------------------------------------------------------
-- Autocommands
----------------------------------------------------------------------

-- Map q to quit all from the starter screen
vim.api.nvim_create_autocmd("User", {
	pattern = "MiniStarterOpened",
	callback = function(ev)
		vim.keymap.set("n", "q", "<cmd>qa<cr>", { buffer = ev.buf, desc = "Quit all" })
	end,
})

-- Delete the starter buffer when navigating away (e.g. :J, :S, opening a file).
-- Without this, the starter tab lingers and breaks gt cycling since most
-- keymaps don't work in the ministarter filetype.
vim.api.nvim_create_autocmd("BufLeave", {
	callback = function(ev)
		if vim.bo[ev.buf].filetype == "ministarter" then
			vim.schedule(function()
				if vim.api.nvim_buf_is_valid(ev.buf) then
					vim.api.nvim_buf_delete(ev.buf, { force = true })
				end
			end)
		end
	end,
})

-- Re-open starter when the last real buffer is closed (e.g. quitting lazygit,
-- :J, tuicr, or closing the last file). Fires on BufEnter (landing on an empty
-- buffer after a tab/window close) and TermClose (terminal exits). Uses
-- vim.schedule to let cleanup settle before checking state.
local function maybe_open_starter()
	local cur = vim.api.nvim_get_current_buf()
	if not vim.api.nvim_buf_is_valid(cur) then
		return
	end
	-- Current buffer is something real — don't interfere
	if vim.bo[cur].filetype ~= "" or vim.bo[cur].buftype ~= "" or vim.api.nvim_buf_get_name(cur) ~= "" then
		return
	end
	-- Check if any named/typed listed buffers remain
	local real_bufs = vim.tbl_filter(function(b)
		return vim.api.nvim_buf_is_valid(b)
			and vim.bo[b].buflisted
			and (vim.api.nvim_buf_get_name(b) ~= "" or vim.bo[b].filetype ~= "")
	end, vim.api.nvim_list_bufs())
	if #real_bufs == 0 then
		starter.open()
	end
end

vim.api.nvim_create_autocmd({ "BufEnter", "TermClose" }, {
	callback = function()
		vim.schedule(maybe_open_starter)
	end,
})
