# debrief

Config for [debrief](https://github.com/martintrojer/debrief), the TUI for catching up with agent commits and answering back. It lives at `debrief/.config/debrief/config.toml`, seeded from `debrief --init-config`; uncommented lines are what differs from debrief's defaults.

- `view.layout = "split"`: the commit view starts side by side (`t` toggles); below `split_min_width` (140 columns) it falls back to unified. The file list stays on the left, the default.
- `view.sidebar_commits = true`: the sidebar is split, file tree on top (60%) and the log's commits below, like ramble's `split` sidebar (`<leader>E` toggles; `C-j`/`C-k` move between them).
- `view.tmux_zoom = true`: inside tmux, debrief zooms its pane at start (as `PREFIX z`) and unzooms on exit. It leaves the window alone if it was already zoomed at start, or if you unzoomed it in the meantime.

`debrief --init-config --config /tmp/debrief.toml` writes the full commented default list to compare against.
