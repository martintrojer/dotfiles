local sessions = require("sessions")

return function(map)
	map("n", "<leader>Sp", sessions.pick, { desc = "Pick session label" })
	map("n", "<leader>Ss", sessions.save, { desc = "Save default session" })
	map("n", "<leader>Sl", sessions.load, { desc = "Load default session" })
	map("n", "<leader>Sd", sessions.delete, { desc = "Delete session" })
	map("n", "<leader>Sn", function()
		vim.ui.input({ prompt = "Session label: " }, function(name)
			if name and name ~= "" then
				sessions.save(name)
			end
		end)
	end, { desc = "Save named session label" })
end
