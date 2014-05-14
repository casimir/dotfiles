eval `ssh-agent`
setxkbmap dvorak -option "compose:102"
xmodmap ~/.keymaps/swapcapsesc
xmodmap ~/.keymaps/swedish-aoe
xset b off
xset r rate 150
xflux -l 57.70887 -g 11.97456
urxvt +sb &
autocutsel -selection PRIMARY -selection SECONDARY -fork &
exec xmonad
