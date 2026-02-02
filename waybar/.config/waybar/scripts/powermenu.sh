#!/bin/bash

chosen=$(echo -e "󰌾 Lock\n󰜺 Reboot\n󰗽 Logout\n󰐥 Shutdown\n󰤄 Suspend" | fuzzel --dmenu --prompt "Power Menu  " --width 25 --lines 5)

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
