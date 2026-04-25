----------------------------------------------------------------------
-- Module References
----------------------------------------------------------------------
local zk_commands = require("zk.commands")

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- Call a Zk command with an opts table, bypassing the Ex parser.
--
-- The user-defined `:ZkNew {…}` command parses its args via
-- `loadstring("return " .. args)()`, so embedding user input
-- (e.g. a note title containing `'`) in the Ex form is unsafe.
-- `commands.get(name)` returns the underlying callback that takes
-- the table directly, sidestepping the eval entirely.
local function zk_call(name, opts)
	local handler = zk_commands.get(name)
	if not handler then
		vim.notify("zk command not registered: " .. name, vim.log.levels.ERROR)
		return
	end
	handler(opts)
end

local function np()
	return vim.g.notes_path
end

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
return function(map)
	map("n", "<leader>zn", function()
		vim.ui.input({ prompt = "Title: " }, function(title)
			if title and title ~= "" then
				zk_call("ZkNew", { notebook_path = np(), title = title, group = "inbox" })
			end
		end)
	end, { desc = "New Note" })

	map("n", "<leader>zN", function()
		vim.ui.input({ prompt = "Title: " }, function(title)
			if title and title ~= "" then
				zk_call("ZkNew", { notebook_path = np(), title = title })
			end
		end)
	end, { desc = "New Permanent Note" })

	map("n", "<leader>zw", function()
		zk_call("ZkNew", { notebook_path = np(), group = "journal" })
	end, { desc = "Weekly Journal" })

	map("n", "<leader>zf", function()
		zk_call("ZkNotes", { notebook_path = np(), sort = { "modified" } })
	end, { desc = "Find Notes" })

	map("n", "<leader>zz", function()
		zk_call("ZkTags", { notebook_path = np() })
	end, { desc = "Find by Tag" })

	map("n", "<leader>zs", function()
		vim.ui.input({ prompt = "Search: " }, function(query)
			if query and query ~= "" then
				zk_call("ZkNotes", {
					notebook_path = np(),
					sort = { "modified" },
					match = { query },
				})
			end
		end)
	end, { desc = "Search Notes" })

	map("n", "<leader>zl", function()
		zk_call("ZkLinks", { notebook_path = np() })
	end, { desc = "Linked Notes" })

	map("n", "<leader>zb", function()
		zk_call("ZkBacklinks", { notebook_path = np() })
	end, { desc = "Backlinks" })
end
