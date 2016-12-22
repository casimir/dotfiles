
if [[ "$HOSTNAME" == "gunka" ]]; then
    xrandr | grep 'VGA. connected' && xrandr --output LVDS1 --off --output VGA1 --auto
    xrandr | grep 'VGA. connected' && xrandr --output LVDS2 --off --output VGA2 --auto
    rm .log
    (iwlist wlp3s0 scan | grep TN_private_3A7M77 && wishful >> ~/.log & (sleep 10; sudo dhcpcd wlp3s0 >> ~/.log &)) &
    lsusb | grep Kinesis >/dev/null && gunka-advantage
fi

if [[ "$HOSTNAME" == "melocoton" ]]; then
    xrandr | grep 'DP1-2 connected' && xrandr --output eDP1 --off --output DP1-2 --auto
fi

disown -a
eval `ssh-agent`

setxkbmap dvorak -option "compose:102"
xmodmap ~/.keymaps/swapcapsesc
xmodmap ~/.keymaps/swedish-aoe
xmodmap ~/.keymaps/mode_switch_alt_gr

xflux -l 57.70887 -g 11.97456

xrdb .Xresources
urxvt &

xset b off
xset r rate 150

exec xmonad
