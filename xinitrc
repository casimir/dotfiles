
cd ~/.keymaps
cape swapcapsesc
xmodmap mode_switch_alt_gr
if [[ "$HOSTNAME" == "gunka" ]]; then
    lsusb | grep Kinesis >/dev/null && cape fix-gunka-advantage
fi
if [[ "$HOSTNAME" == "melocoton" ]]; then
    lsusb | grep Kinesis >/dev/null && cape fix-advantage
fi

cd ~

if [[ "$HOSTNAME" == "gunka" ]]; then
    xrandr | grep 'VGA. connected' && xrandr --output LVDS1 --auto --output VGA1 --auto --right-of LVDS1
fi
if [[ "$HOSTNAME" == "melocoton" ]]; then
    xrandr | grep 'DP1-2 connected' && xrandr --output eDP1 --off --output DP1-2 --auto
fi

eval `ssh-agent`

xflux -l 57.70887 -g 11.97456

xrdb .Xresources
urxvt &

xset b off
xset r rate 150

xsetroot -solid '#1c1c1c'

exec xmonad
