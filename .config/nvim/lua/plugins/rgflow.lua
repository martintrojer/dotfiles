-- vim.keymap.set("n", "<leader>rr", function()
-- 	require("lazy.core.loader").reload("rgflow.nvim")
-- end, { desc = "Reload RgFlow" })

return {
	"martintrojer/rgflow.nvim",
	opts = {
		-- Mappings to trigger RgFlow functions
		default_trigger_mappings = true,
		-- These mappings are only active when the RgFlow UI (panel) is open
		default_ui_mappings = true,
		-- QuickFix window only mapping
		default_quickfix_mappings = true,
		quickfix = {
			max_height_lines = 15,
		}
	},
}
