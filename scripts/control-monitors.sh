#!/usr/bin/env bash

source $XMONAD_HOME/scripts/check_cmd.sh
check_cmd arandr arandr

intern="eDP-1"
extern="HDMI-1-0"

case "$1" in
    arandr)
        # run arandr GUI
        arandr
        ;;
    normal)
        # default, single eDP
        xrandr --output $intern --auto --output $extern --off
        notify-send "Monitors Mode" "$intern only"
        ;;
    # hdmi)
    #     # single HDMI
    #     xrandr --output $intern --auto --output $extern --auto --primary
    #     notify-send "Monitors Mode" "$extern only"
    #     ;;
    dual)
        # dual display
        xrandr --output $intern --auto --output $extern --auto --primary --right-of $intern
        notify-send "Monitors Mode" "$intern || $extern"
        ;;
    mirror)
        # mirror
        xrandr --output $intern --auto --pos 0x0 --output $extern --auto --pos 0x0
        notify-send "Monitors Mode" "$extern <-> $intern"
        ;;
    up-down)
        # up-down
        xrandr --output $intern --auto --output $extern --auto --primary --below $intern
        notify-send "Monitors Mode" "$intern \n=======\n$extern"
        ;;
    *)
        # default, single eDP
        xrandr --output $intern --auto --output $extern --off
        notify-send "Monitors Mode" "$intern only"
        ;;
esac
exit 0
