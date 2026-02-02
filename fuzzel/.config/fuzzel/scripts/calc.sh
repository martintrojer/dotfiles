#!/bin/bash
# Calculator using qalc or bc

answer=""
while true; do
    query=$(echo "$answer" | fuzzel --dmenu --prompt "Calc  " --width 40)

    # Exit if empty or cancelled
    [ -z "$query" ] && break

    # Calculate using qalc (preferred) or bc
    if command -v qalc &>/dev/null; then
        answer=$(qalc -t "$query" 2>/dev/null)
    else
        answer=$(echo "$query" | bc -l 2>/dev/null)
    fi

    # Copy result to clipboard
    if [ -n "$answer" ]; then
        echo -n "$answer" | wl-copy
        notify-send "Calculator" "$query = $answer (copied)"
    fi
done
