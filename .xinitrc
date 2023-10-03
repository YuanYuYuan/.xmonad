#!/bin/sh

# Copy from /etc/X11/xinit/xinitrc to prevent GTK hang
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Input method
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
fcitx5 &

dunst &
udiskie &
greenclip daemon &
picom -b
redshift &
nm-applet &

xset r rate 250 50
xrandr --output HDMI-0 --primary --mode 1920x1080
xmodmap $HOME/.Xmodmap

# set xcursor
xsetroot -cursor_name left_ptr

feh --bg-scale $HOME/.xmonad/pic/wallpaper.jpg

# flash window while chaning focus
flashfocus &

# hide mouse
unclutter -idle 10 &

# bluetooth
blueman-applet &

exec xmonad
# exec xfce4-session
