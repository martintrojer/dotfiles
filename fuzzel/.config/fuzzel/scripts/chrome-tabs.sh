#!/bin/bash
# Chrome tab switcher using Chrome DevTools Protocol
# Searches both main (9222) and private (9223) Chrome profiles

CDP_PORTS="${CDP_PORTS:-9222 9223}"
CDP_GLYPHS=("○" "●")

# Collect tabs from all ports
all_tabs=""
port_idx=0
for port in $CDP_PORTS; do
    json_data=$(curl -s "http://localhost:$port/json" 2>/dev/null)
    if [ -n "$json_data" ] && [ "$json_data" != "[]" ]; then
        # Filter and add port info to each tab
        tabs=$(echo "$json_data" | jq -c --arg port "$port" --arg glyph "${CDP_GLYPHS[$port_idx]}" \
            '[.[] | select(.type == "page") | select(.url | startswith("chrome://") | not) | select(.url | startswith("chrome-extension://") | not) | select(.url | startswith("devtools://") | not) | . + {port: $port, glyph: $glyph}]')
        if [ "$tabs" != "[]" ]; then
            if [ -z "$all_tabs" ]; then
                all_tabs="$tabs"
            else
                # Merge arrays
                all_tabs=$(echo "$all_tabs $tabs" | jq -s 'add')
            fi
        fi
    fi
    port_idx=$((port_idx + 1))
done

if [ -z "$all_tabs" ] || [ "$all_tabs" = "[]" ] || [ "$all_tabs" = "null" ]; then
    notify-send "Chrome Tabs" "No Chrome instances running with debug ports"
    exit 1
fi

tab_count=$(echo "$all_tabs" | jq 'length')
if [ "$tab_count" -eq 0 ]; then
    notify-send "Chrome Tabs" "No tabs open"
    exit 0
fi

# Build display list: glyph title | url
display=$(echo "$all_tabs" | jq -r '.[] | "\(.glyph) \(.title) | \(.url | if length > 50 then .[0:47] + "..." else . end)"')

# Show fuzzel and get selection
selected=$(echo "$display" | fuzzel --dmenu --prompt "Chrome  " --width 85 --lines 15)

if [ -n "$selected" ]; then
    # Find index by line number match
    idx=$(echo "$display" | grep -nxF "$selected" | head -1 | cut -d: -f1)

    # Fallback: partial match
    if [ -z "$idx" ]; then
        selected_start=$(echo "$selected" | head -c 40)
        idx=$(echo "$display" | grep -n "^$selected_start" | head -1 | cut -d: -f1)
    fi

    if [ -n "$idx" ]; then
        idx=$((idx - 1))

        # Get tab info from the combined JSON
        tab_id=$(echo "$all_tabs" | jq -r ".[$idx].id")
        tab_title=$(echo "$all_tabs" | jq -r ".[$idx].title")
        tab_port=$(echo "$all_tabs" | jq -r ".[$idx].port")

        if [ -n "$tab_id" ] && [ "$tab_id" != "null" ]; then
            # Activate the tab in Chrome (retry up to 3 times)
            for attempt in 1 2 3; do
                result=$(curl -s "http://localhost:$tab_port/json/activate/$tab_id" 2>/dev/null)
                [ -n "$result" ] && break
                sleep 0.05
            done

            # Wait for Chrome to update window title, then find window by title
            for _ in 1 2 3 4 5; do
                sleep 0.1
                chrome_window=$(niri msg -j windows | jq -r --arg title "$tab_title" \
                    '.[] | select(.app_id | test("chrome|chromium"; "i")) | select(.title | startswith($title)) | .id' | head -1)
                [ -n "$chrome_window" ] && break
            done

            # Fallback: just pick first Chrome window
            if [ -z "$chrome_window" ]; then
                chrome_window=$(niri msg -j windows | jq -r '.[] | select(.app_id | test("chrome|chromium"; "i")) | .id' | head -1)
            fi

            if [ -n "$chrome_window" ]; then
                niri msg action focus-window --id "$chrome_window"
            fi
        fi
    fi
fi
