----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
return function(map)
	----------------------------------------------------------------------
	-- Helpers
	----------------------------------------------------------------------
	local np = function()
		return vim.g.notes_path
	end

	----------------------------------------------------------------------
	-- Note Keymaps
	----------------------------------------------------------------------
	map("n", "<leader>zn", function()
		vim.ui.input({ prompt = "Title: " }, function(title)
			if title then
				vim.cmd("ZkNew { notebook_path = '" .. np() .. "', title = '" .. title .. "', group = 'inbox' }")
			end
		end)
	end, { desc = "New Note" })
	map("n", "<leader>zN", function()
		vim.ui.input({ prompt = "Title: " }, function(title)
			if title then
				vim.cmd("ZkNew { notebook_path = '" .. np() .. "', title = '" .. title .. "' }")
			end
		end)
	end, { desc = "New Permanent Note" })
	map("n", "<leader>zw", function()
		vim.cmd("ZkNew { notebook_path = '" .. np() .. "', group = 'journal' }")
	end, { desc = "Weekly Journal" })
	map("n", "<leader>zf", function()
		vim.cmd("ZkNotes { notebook_path = '" .. np() .. "', sort = { 'modified' } }")
	end, { desc = "Find Notes" })
	map("n", "<leader>zz", function()
		vim.cmd("ZkTags { notebook_path = '" .. np() .. "' }")
	end, { desc = "Find by Tag" })
	map("n", "<leader>zs", function()
		vim.ui.input({ prompt = "Search: " }, function(query)
			if query then
				vim.cmd(
					"ZkNotes { notebook_path = '" .. np() .. "', sort = { 'modified' }, match = { '" .. query .. "' } }"
				)
			end
		end)
	end, { desc = "Search Notes" })
	map("n", "<leader>zl", function()
		vim.cmd("ZkLinks { notebook_path = '" .. np() .. "' }")
	end, { desc = "Linked Notes" })
	map("n", "<leader>zb", function()
		vim.cmd("ZkBacklinks { notebook_path = '" .. np() .. "' }")
	end, { desc = "Backlinks" })
end
