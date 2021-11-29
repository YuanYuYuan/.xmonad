#!/usr/bin/env bash

source $XMONAD_HOME/scripts/check_cmd.sh
check_cmd amixer alsa-utils
check_cmd dunstify dunst

function get_volume {
    amixer get Master | grep '%' | head -n 1 | cut -d '[' -f 2 | cut -d '%' -f 1
}

function is_mute {
    amixer get Master | grep '%' | grep -oE '[^ ]+$' | grep off > /dev/null
}

# function send_notification {
#     if is_mute; then
#         text="mute"
#         icon="audio-volume-muted-symbolic"
#     else
#         val=$(get_volume)
#         if (( $val >= 0 && $val < 33 )); then
#             icon="audio-volume-low-symbolic"
#         elif (( $val >= 33 && $val < 66 )); then
#             icon="audio-volume-medium-symbolic"
#         else
#             icon="audio-volume-high-symbolic"
#         fi
#         text="$val $(seq --separator="─" 0 "$((val / 5))" | sed 's/[0-9]//g')"
#     fi
#     dunstify -t 600 -i $icon -r 2593 -u normal "$text"
# }

function send_notification {
    if is_mute; then
        text=" Muted"
    else
        val=$(get_volume)
        text="墳 $val $(seq --separator="─" 0 "$((val / 5))" | sed 's/[0-9]//g')"
    fi
    dunstify -t 600 -r 2593 -u normal "$text"
}

case $1 in
    up)
        amixer set Master on > /dev/null
        amixer set Master 5%+ > /dev/null
        ;;
    down)
        amixer set Master on > /dev/null
        amixer set Master 5%- > /dev/null
        ;;
    mute)
        amixer set Master 1+ toggle > /dev/null
        ;;
esac

send_notification
