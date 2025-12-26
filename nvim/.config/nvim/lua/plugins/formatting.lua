return {
  "stevearc/conform.nvim",
  opts = {
    formatters_by_ft = {
      ["*"] = { "trim_whitespace", "trim_newlines" },
    },
    default_format_opts = {
      timeout_ms = 500,
      lsp_fallback = true, -- Use LSP if no formatter is found
    },
  },
}
