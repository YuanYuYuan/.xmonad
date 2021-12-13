#!/usr/bin/env bash

source $XMONAD_HOME/scripts/check_cmd.sh
check_cmd import imagemagick

case $1 in
    "")
        import -trim /tmp/screenshot.png
        xclip -selection clipboard -target image/png -in < /tmp/screenshot.png
        ;;
    *)
        import -trim ${1}.png
        ;;
esac


