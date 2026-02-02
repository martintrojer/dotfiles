#!/bin/bash
# Window switcher using niri msg

# Get windows as JSON and format for fuzzel
windows=$(niri msg -j windows | jq -r '.[] | "\(.id)\t\(.app_id // "unknown")\t\(.title // "untitled")"')

if [ -z "$windows" ]; then
    notify-send "Windows" "No windows open"
    exit 0
fi

# Format for display: "app_id: title"
display=$(echo "$windows" | awk -F'\t' '{print $2 ": " $3}')

selected=$(echo "$display" | fuzzel --dmenu --prompt "Windows  " --width 60)

if [ -n "$selected" ]; then
    # Find the window ID from the selection
    app_id=$(echo "$selected" | cut -d':' -f1)
    title=$(echo "$selected" | cut -d':' -f2- | sed 's/^ //')

    window_id=$(echo "$windows" | awk -F'\t' -v app="$app_id" -v t="$title" '$2 == app && $3 == t {print $1; exit}')

    if [ -n "$window_id" ]; then
        niri msg action focus-window --id "$window_id"
    fi
fi
