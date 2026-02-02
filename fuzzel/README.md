# Fuzzel

## Scripts

- `fuzzel/.config/fuzzel/scripts/ssh.sh` - SSH host selector
- `fuzzel/.config/fuzzel/scripts/calc.sh` - Calculator with qalc/bc
- `fuzzel/.config/fuzzel/scripts/windows.sh` - Window switcher (niri)
- `fuzzel/.config/fuzzel/scripts/clipboard.sh` - Clipboard history via clipman
- `fuzzel/.config/fuzzel/scripts/emoji.sh` - Emoji picker with bemoji

## Script Pattern

All fuzzel scripts follow this convention:

```bash
#!/bin/bash
# Description comment

# 1. Gather options (from command output, config files, or APIs)
options=$(source_command)

# 2. Display selection dialog
selected=$(echo "$options" | fuzzel --dmenu --prompt "Label  " --width 40)

# 3. Execute action based on selection
if [ -n "$selected" ]; then
    action_command "$selected"
fi
```

### Common Parameters

- `--dmenu` - Enable dmenu mode (read from stdin)
- `--prompt "Text  "` - Set prompt with styling
- `--width 40-80` - Width as percentage
- `--lines 5-15` - Number of visible lines

### Conventions

- Use `notify-send` for user-facing error messages
- Use `wl-copy` for Wayland clipboard operations
- Use `jq` for JSON parsing (see windows.sh)
- Support fallback commands where possible (see calc.sh)

## Configuration

Located at `fuzzel/.config/fuzzel/fuzzel.ini`:

- Font: JetBrains Mono Nerd Font, size 14
- Terminal: ghostty
- Theme: Catppuccin Mocha
- Icons: Papirus-Dark
