#!/usr/bin/env bash

case $1 in
    "")
        import -trim /tmp/screenshot.png
        xclip -selection clipboard -target image/png -in < /tmp/screenshot.png
        ;;
    *)
        import -trim ${1}.png
        ;;
esac


