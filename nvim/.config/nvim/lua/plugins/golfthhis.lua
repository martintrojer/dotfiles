return {
  "martintrojer/golf-this.nvim",
  dependencies = {
    "MunifTanjim/nui.nvim",
    "nvim-lua/plenary.nvim",
  },
  config = function()
    require("golf_this").setup({
      provider = "openrouter",
      include_in_prompt = "use lazyvim key bindings",
      providers = {
        openrouter = {
          model = "google/gemini-3-flash-preview",
        },
      },
    })
  end,
}
