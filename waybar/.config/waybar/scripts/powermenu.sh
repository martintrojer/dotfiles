#!/bin/bash

chosen=$(echo -e "󰌾 Lock\n󰜺 Reboot\n󰗽 Logout\n󰐥 Shutdown\n󰤄 Suspend" | wofi --dmenu -i -O default --width 250 --height 340 --prompt "Power Menu")

case "$chosen" in
    *Lock*)
        loginctl lock-session
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
        niri msg action quit -s
        ;;
    *)
        ;;
esac
