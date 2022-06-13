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
exec xmonad
