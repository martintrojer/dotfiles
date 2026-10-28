# Ghostty

Ghostty is the terminal on both OSes. One shared config plus a per-OS half:

| File | Package | Linked on |
|---|---|---|
| `ghostty/.config/ghostty/config` | `ghostty` | both |
| `ghostty-linux/.config/ghostty/os.ghostty` | `ghostty-linux` | Linux |
| `ghostty-macos/.config/ghostty/os.ghostty` | `ghostty-macos` | macOS |

The shared file loads `os.ghostty` last (`config-file = ?os.ghostty`), so the OS half wins on conflicts. Shared terminal behaviour lives in `tmux/`, `zsh/`, `nvim/`; keep these files to rendering, input translation, clipboard and window creation.

## macOS

The Homebrew cask installs it. Hammerspoon's terminal binds (`Hyper+T`, `Hyper+Return`, `Hyper+PadEnter`) all use `open -na "Ghostty"` to spawn current-Space windows without swooshing macOS to existing windows on other Spaces.

## Linux

Fedora has no package, so `build-ghostty.py` builds a release from source:

```sh
ghostty/build-ghostty.py --latest   # or an explicit tag: 1.4.0
```

- Only signed release tarballs: minisign signature checked against upstream's key, Zig's sha256 against ziglang.org. Tip and git checkouts are refused.
- Builds in the `ghostty-build` toolbox (created on first run), so -devel packages stay off the host.
- Installs to `~/.local/opt/ghostty-<ver>` and points `~/.local/bin/ghostty` at it. Old prefixes stay until removed by hand, which makes rollback one `ln -sf`.
- gtk4-layer-shell is copied into the prefix's `lib/` and found via RPATH; Sericea ships only the gtk3 one.

Sway launches plain `ghostty`, one process per window with no daemon. The floating popups (wiremix, bluetui, nmtui, btop, yazi) pass `--class=ghostty.<name>-float`; GTK rejects a class without a dot. Ghostty sets `TERMINFO` itself, so `xterm-ghostty` resolves without a system terminfo entry.

Images use the Kitty graphics protocol through tmux passthrough; Ghostty has no Sixel. yazi detects it on its own. See `docs/DECISIONS.md`, "Ghostty on Linux too", for the measurements behind the font settings and the plain-launch choice.
