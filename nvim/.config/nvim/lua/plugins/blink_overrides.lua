return {
  "saghen/blink.cmp",
  opts = {
    sources = {
      per_filetype = {
        -- buffer,snippets deliberately removed from defaults
        markdown = { "lsp", "path" },
      },
    },
  },
}
