----------------------------------------------------------------------
-- Module References
----------------------------------------------------------------------
local todos = require("todos")
local util = require("util")

local function quickfix_replace()
	vim.ui.input({ prompt = "Replace: " }, function(old)
		if not old or old == "" then
			return
		end
		vim.ui.input({ prompt = "With: " }, function(new)
			if not new then
				return
			end
			vim.cmd("cfdo %s/" .. vim.fn.escape(old, "/") .. "/" .. vim.fn.escape(new, "/") .. "/gc")
		end)
	end)
end

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
return function(map)
	map("n", "<leader>sg", function()
		util.fzf_with_cwd("live_grep", util.buf_dir)
	end, { desc = "Live grep (rg)" })
	map("n", "<leader>s/", function()
		require("fzf-lua").live_grep({ resume = true })
	end, { desc = "Resume live grep" })
	map("n", "<leader>sG", function()
		util.fzf_with_cwd("grep", util.vcs_dir, {
			cmd = "git grep --line-number --color=always",
			prompt = "Git Grep> ",
		})
	end, { desc = "Git grep" })
	map("n", "<leader>sw", function()
		util.fzf_with_cwd("grep_cword", util.buf_dir)
	end, { desc = "Grep word under cursor" })
	map("n", "<leader>st", function()
		todos.grep({ cwd = util.buf_dir() })
	end, { desc = "TODOs/FIXes (buffer dir)" })
	map("n", "<leader>sT", function()
		todos.grep({ cwd = util.vcs_dir() })
	end, { desc = "TODOs/FIXes (repo root)" })
	map("n", "<leader>sr", function()
		util.fzf_with_cwd("live_grep", util.buf_dir, { prompt = "Search (Tab select → Enter → cfdo)> " })
	end, { desc = "Search & replace (grep → quickfix)" })
	map("n", "<leader>sR", quickfix_replace, { desc = "Replace in quickfix files" })

	-- Call vecgrep.search() directly: the :Vecgrep Ex command splits its
	-- args on `|` and `<bar>`, which breaks user input containing pipes or
	-- newlines (especially visual selections).
	map("n", "<leader>sv", function()
		vim.ui.input({ prompt = "Vecgrep: " }, function(query)
			if query and query ~= "" then
				require("vecgrep").search(query)
			end
		end)
	end, { desc = "Semantic search" })
	map("v", "<leader>sv", function()
		local text = table.concat(vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getpos(".")), " ")
		if text ~= "" then
			require("vecgrep").search(text)
		end
	end, { desc = "Semantic search selection" })
	map("n", "<leader>sV", "<Cmd>VecgrepLive<CR>", { desc = "Live semantic search" })
	map("n", "<leader>sX", "<Cmd>VecgrepReindex<CR>", { desc = "Reindex vecgrep" })
end
