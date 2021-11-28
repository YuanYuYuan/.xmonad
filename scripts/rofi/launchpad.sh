#!/usr/bin/env bash

config_path="$XMONAD_HOME/scripts/rofi/launchpad.rasi"
rofi \
    -no-lazy-grab \
    -show drun \
    -modi drun \
    -theme $config_path
