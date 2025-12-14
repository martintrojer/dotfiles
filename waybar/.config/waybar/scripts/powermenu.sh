#!/bin/bash

chosen=$(echo -e "󰤄 Suspend\n󰐥 Shutdown\n󰜺 Reboot\n󰗽 Logout" | wofi --dmenu --width 200 --height 250 --prompt "Power Menu")

case "$chosen" in
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
