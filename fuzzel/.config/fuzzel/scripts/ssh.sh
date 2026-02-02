#!/bin/bash
# SSH host selector from ~/.ssh/config

hosts=$(grep -E "^Host [^*]" ~/.ssh/config 2>/dev/null | cut -d' ' -f2- | tr ' ' '\n' | sort -u)

if [ -z "$hosts" ]; then
    notify-send "SSH" "No hosts found in ~/.ssh/config"
    exit 1
fi

selected=$(echo "$hosts" | fuzzel --dmenu --prompt "SSH  " --width 40)
if [ -n "$selected" ]; then
    ghostty -e ssh "$selected"
fi
