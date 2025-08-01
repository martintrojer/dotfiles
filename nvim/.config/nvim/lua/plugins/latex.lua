return {
  "jbyuki/nabla.nvim",
  event = "VeryLazy",

  config = function()
    vim.keymap.set("n", "<leader>pp", function()
      require("nabla").popup()
    end, { desc = "Latex popup" })
  end,
}
