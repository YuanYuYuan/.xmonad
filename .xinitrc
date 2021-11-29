# Input method
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
fcitx5 &

dunst &
greenclip daemon &
picom -b
xset r rate 300 50
xrandr --output HDMI-0 --primary --mode 1920x1080
feh --bg-scale $HOME/.xmonad/pic/wallpaper.jpg
exec xmonad
