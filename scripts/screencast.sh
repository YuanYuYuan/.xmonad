#!/usr/bin/env bash

timestamp=$(date +"%Y-%m-%d.%T")
file_name="$HOME/Videos/${timestamp}.mkv"
notify-send "Start screen recording..."
ffmpeg -video_size 1920x1080 -framerate 25 -f x11grab -i :0.0+0,0 -f pulse -ac 2 -i default $file_name
notify-send "Screencast had been stored in $file_name!"
