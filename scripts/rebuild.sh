#!/usr/bin/env bash

notify-send "Recompiling..."
if xmonad --recompile; then
    notify-send "done! Restarted."
    xmonad --restart
else
    notify-send "failed!"
fi
