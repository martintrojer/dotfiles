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
	local hour = tonumber(os.date("%H"))
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
			vim.cmd("ZkNotes { notebook_path = '" .. vim.g.notes_path .. "', sort = { 'modified' } }")
		end),
		action_item(notes_header, "Journal", function()
			vim.cmd("ZkNew { notebook_path = '" .. vim.g.notes_path .. "', group = 'journal' }")
		end),
		action_item(notes_header, "TODOs", function()
			require("grep-todos").grep({ cwd = vim.g.notes_path })
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
vim.api.nvim_create_autocmd("User", {
	pattern = "MiniStarterOpened",
	callback = function(ev)
		vim.keymap.set("n", "q", "<cmd>qa<cr>", { buffer = ev.buf, desc = "Quit all" })
	end,
})

-- Close the starter buffer when leaving it so it doesn't linger in tabs
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
