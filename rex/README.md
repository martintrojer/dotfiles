# Rex

Rex (`/Applications/Rex Beta.app`) settings, mirrored from `ghostty/`. Only `.config/rex/init.lua` is linked.

Rex keeps its settings in macOS user defaults (`com.superlogical.rex`), next to window frames, session state and update timestamps, so a tracked plist would churn. `_dotfiles_sync/rex_settings.py` owns a few keys. `--check` reports drift and `--apply` writes those keys. Everything else is left alone.

| Setting | Value | Ghostty equivalent |
|---|---|---|
| `Workspace.terminalFontName` | `GeistMono Nerd Font Mono` | `font-family` |
| `Workspace.terminalFontPointSize` | `16` | `font-size` |
| `Workspace.terminalAllowsLigatures` | `false` | `font-feature = -liga` |
| `Workspace.inactivePaneFadeMode` | `surface` | `unfocused-split-opacity` (subtle: text stays full strength) |
| `Workspace.darkThemeID` | the theme below | `theme` |

`catppuccin-mocha.rex-theme.json` is the entry Rex wrote to `Workspace.customThemes` after importing Ghostty's `Catppuccin Mocha` (Settings → Appearance, or `client.theme.import`). `--apply` adds it or replaces it by `id`, and any other custom themes stay. To re-export it after re-importing in the app:

```sh
defaults export com.superlogical.rex /tmp/rex.plist
python3 -c 'import json,plistlib; t=json.loads(plistlib.load(open("/tmp/rex.plist","rb"))["Workspace.customThemes"]); print(json.dumps(next(x for x in t if x["name"]=="Catppuccin Mocha"),indent=2))' > rex/catppuccin-mocha.rex-theme.json
```

Rex caches settings while it runs, so quit it before `dotfiles-sync --apply` and start it again afterwards.

Known issue: pi's coloured sections (tool blocks) can bleed into the blank rows between them in Rex but not in Ghostty. `rex capture --format=vt` shows those rows have no background, so Rex stores the screen correctly. Static blocks and a scripted imitation of pi's redraw both render correctly, and turning smooth scrolling off made no difference. Unresolved.

Ghostty's opacity, blur and `font-thicken` have no Rex key. The fade modes seen in the app are `off`, `surface`, `content` and `both`.

`.config/rex/init.lua` is linked to `~/.config/rex/init.lua` and holds key bindings only. Keep it small and lean on Rex's own defaults. Rex doesn't watch the file, so run `rex config reload` after a change and `rex keymap` to see what loaded.

| Key | Does |
|---|---|
| `ctrl+h/j/k/l` | Focus the pane left/down/up/right. If the pane runs nvim, vim or tmux, the key goes to that program instead. nvim moves between its own splits and at its edge runs `rex focus DIR` (`nvim/.config/nvim/lua/keymaps/core.lua`), the same handoff vim-tmux-navigator does for tmux |

The check reads the pane's foreground process, so debrief and ramble lose their internal pane movement here: their edge handoff calls `tmux select-pane`.
