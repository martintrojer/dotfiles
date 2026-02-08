# Fuzzel

## Scripts

- `fuzzel/.config/fuzzel/scripts/ssh` - SSH host selector
- `fuzzel/.config/fuzzel/scripts/calc` - Calculator with qalc/bc
- `fuzzel/.config/fuzzel/scripts/windows` - Window switcher (niri)
- `fuzzel/.config/fuzzel/scripts/hotkeys` - Niri keybindings picker
- `fuzzel/.config/fuzzel/scripts/clipboard` - Clipboard history via clipman
- `fuzzel/.config/fuzzel/scripts/emoji` - Emoji picker with bemoji
- `fuzzel/.config/fuzzel/scripts/powermenu` - Power menu (lock/suspend/logout/reboot/shutdown)
- `fuzzel/.config/fuzzel/scripts/raise-window` - Focus a niri window by id

`raise-window` supports:
- `--id <window_id>` for direct focus
- `--app-id-regex` and/or `--title-regex` to find and focus a matching window
- `--spawn-sh '<cmd>'` to launch fallback command when no match is found

## Script Pattern

All fuzzel scripts are Python 3 (`#!/usr/bin/env python3`) and follow this convention:

```python
#!/usr/bin/env python3
import argparse

from _common import fuzzel_dmenu

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--prompt", default="Label  ")
    parser.add_argument("--width", type=int, default=40)
    return parser.parse_args()

def main():
    args = parse_args()
    selected = fuzzel_dmenu(prompt=args.prompt, width=args.width, options=[...])
    if selected:
        ...
```

### Common Parameters

- `--dmenu` - Enable dmenu mode (read from stdin)
- `--prompt "Text  "` - Set prompt with styling
- `--width 40-80` - Width as percentage
- `--lines 5-15` - Number of visible lines

### Conventions

- Use `notify-send` for user-facing error messages
- Use `wl-copy` for Wayland clipboard operations
- Use stdlib modules like `argparse`, `json`, `urllib`, `subprocess`
- Share common helpers from `fuzzel/.config/fuzzel/scripts/_common.py`
- Most-used sorting uses fuzzel `--cache` per picker in `~/.cache/fuzzel/pickers/*.cache` (or `$XDG_CACHE_HOME/fuzzel/pickers/*.cache`)

## Quality Checks

Python scripts in this package should pass:

- `ruff check`
- `ty check`
- `ruff format`

## Configuration

Located at `fuzzel/.config/fuzzel/fuzzel.ini`:

- Font: JetBrains Mono Nerd Font, size 14
- Terminal: ghostty
- Theme: Catppuccin Mocha
- Icons: Papirus-Dark
