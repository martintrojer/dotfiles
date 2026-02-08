#!/bin/bash
# Show Niri keybindings in a fuzzel picker.

config_file="${XDG_CONFIG_HOME:-$HOME/.config}/niri/config.kdl"

if [ ! -f "$config_file" ]; then
    notify-send "Hotkeys" "Niri config not found: $config_file"
    exit 1
fi

entries=$(
    awk '
    function trim(s) {
        gsub(/^[ \t]+|[ \t]+$/, "", s)
        return s
    }

    /^binds[ \t]*\{/ {
        in_binds = 1
        next
    }

    in_binds && /^[ \t]*\}/ {
        exit
    }

    !in_binds {
        next
    }

    /^[ \t]*\/\// || /^[ \t]*$/ {
        next
    }

    /\{/ && /\}/ {
        key = $1
        title = ""
        action = ""

        if (match($0, /hotkey-overlay-title="[^"]+"/)) {
            title = substr($0, RSTART + 22, RLENGTH - 23)
        }

        if (match($0, /\{[^}]+\}/)) {
            action = substr($0, RSTART + 1, RLENGTH - 2)
            action = trim(action)
            sub(/;[ \t]*$/, "", action)
        }

        label = (title != "" ? title : action)
        if (key != "" && label != "") {
            printf "%-24s %s\n", key, label
        }
    }
    ' "$config_file"
)

if [ -z "$entries" ]; then
    notify-send "Hotkeys" "No keybindings found in $config_file"
    exit 1
fi

echo "$entries" | fuzzel --dmenu --prompt "Hotkeys  " --width 80 --lines 20 >/dev/null
