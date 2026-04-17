# Sesh

Repo-managed `sesh` config lives in `sesh/.config/sesh/sesh.toml` and stows to `~/.config/sesh/sesh.toml`.

Install `sesh` itself separately. This package only manages the config; it does not install the binary.

This setup does three things:

- curates a short list of named projects so the picker starts with high-signal entries
- gives new sessions a useful preview via `eza`
- keeps tmux integration lightweight: `prefix + s` for sessions, `prefix + g` for last session, `prefix + T` for project root, `prefix + W` for windows

## Notes

- `sesh` still reads tmux sessions and zoxide results, but config sessions are sorted first
- the blacklist trims obvious home-directory noise such as `.cache`, `.local`, `.config`, and `Library`
- only low-risk startup commands are configured by default (`nvim` for `dotfiles` and `notes`)
