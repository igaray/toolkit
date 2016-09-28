#!/bin/bash
setxkbmap latam

# Swap left control and capslock keys to avoid emacs pinky.
#
# These dont work any more.
# xmodmap -e "remove lock = Caps_Lock"
# xmodmap -e "add control = Caps_Lock"
# xmodmap -e "remove control = Control_L"
# xmodmap -e "add lock = Control_L"

setxkbmap -option ctrl:swapcaps
