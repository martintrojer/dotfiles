#!/bin/bash

chosen=$(echo -e "Shutdown\nReboot\nLogout" | wofi --dmenu --width 200 --height 250 --prompt "Power Menu")

case "$chosen" in
    Shutdown)
        systemctl poweroff
        ;;
    Reboot)
        systemctl reboot
        ;;
    Logout)
        hyprctl dispatch exit
        ;;
    *)
        ;;
esac
