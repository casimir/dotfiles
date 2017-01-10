
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

    #xrandr | grep 'VGA. connected' && xrandr --output LVDS1 --off --output VGA1 --auto
    #xrandr | grep 'VGA. connected' && xrandr --output LVDS2 --off --output VGA2 --auto
    #sudo wpa_supplicant -c /etc/wpa_supplicant.conf -i wlp3s0 -Dnl80211 > .log-wpa &
    #sudo dhcpcd wlp3s0 > .log-dhcp &
    #disown -a
    # lsusb | grep Kinesis >/dev/null && gunka-advantage
#fi

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
