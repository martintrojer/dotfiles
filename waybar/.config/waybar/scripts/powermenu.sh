#!/bin/bash

chosen=$(echo -e "󰌾 Lock\n󰜺 Reboot\n󰗽 Logout\n󰐥 Shutdown\n󰤄 Suspend" | wofi --dmenu -i -O default --width 250 --height 300 --prompt "Power Menu")

case "$chosen" in
    *Lock*)
        if command -v hyprlock &> /dev/null; then
            hyprlock
        elif command -v niri &> /dev/null; then
            niri msg action lock
        fi
        ;;
    *Suspend*)
        systemctl suspend
        ;;
    *Shutdown*)
        systemctl poweroff
        ;;
    *Reboot*)
        systemctl reboot
        ;;
    *Logout*)
        hyprctl dispatch exit
        niri msg action quit -s
        ;;
    *)
        ;;
esac
