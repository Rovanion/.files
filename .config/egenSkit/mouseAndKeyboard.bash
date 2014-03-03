#!/bin/bash

# Setting up scrolling.
synclient HorizTwoFingerScroll=1
synclient HorizScrollDelta=250
synclient PalmDetect=1
synclient VertEdgeScroll=0

# Set the keymap and set caps to be a windows key.
setxkbmap qq dvorak
setxkbmap -option caps:super
