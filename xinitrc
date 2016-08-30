xrandr | grep 'VGA. connected' && xrandr --output LVDS1 --off --output VGA1 --auto
xrandr | grep 'VGA. connected' && xrandr --output LVDS2 --off --output VGA2 --auto
rm .log
(iwlist wlp3s0 scan | grep TN_private_3A7M77 && wishful >> ~/.log & (sleep 10; sudo dhcpcd wlp3s0 >> ~/.log &)) &
(iwlist wlan0 scan | grep TN_private_3A7M77 && wishful2 >> ~/.log & (sleep 10; sudo dhcpcd wlan0 >> ~/.log &)) &
disown -a
eval `ssh-agent`
setxkbmap dvorak -option "compose:102"
xmodmap ~/.keymaps/swapcapsesc
xmodmap ~/.keymaps/swedish-aoe
xmodmap ~/.keymaps/mode_switch_alt_gr
xflux -l 57.70887 -g 11.97456
xrdb .Xresources
urxvt +sb &
autocutsel -selection PRIMARY -selection SECONDARY -fork &
lsusb | grep Kinesis >/dev/null && gunka-advantage
lsusb | grep 'feed:1307' >/dev/null && setxkbmap us && xmodmap .keymaps/ez
xset b off
xset r rate 150
#unclutter -grab &
#xbacklight -set 100 -time 0 -steps 1 &
#exec openbox
exec xmonad
