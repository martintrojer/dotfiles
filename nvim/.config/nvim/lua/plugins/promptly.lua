return {
  "martintrojer/promptly.nvim",
  dependencies = {
    "MunifTanjim/nui.nvim",
    "nvim-lua/plenary.nvim",
  },
  config = function()
    require("promptly").setup({
      providers = {
        openrouter = {
          model = "google/gemini-3-flash-preview",
        },
      },
      profiles = {
        code_assistant = {
          provider = "openrouter",
        },
        golf_this = {
          provider = "openrouter",
          system_message = "You are a Vim golf specialist. Return shortest robust normal-mode sequences and avoid brittle absolute line-number jumps. Assume lazyvim setup, always prefer mini library examples if applicable",
          context = {
            max_context_lines = 250,
            include_current_line = true,
            include_selection = true,
          },
          ui = {
            prompt_title = " Golf Prompt ",
            result_title = " Golf Suggestions ",
          },
          apply = {

            enabled = false,
            allowed_kinds = { "keys" },
          },
        },
      },
    })
  end,
}
