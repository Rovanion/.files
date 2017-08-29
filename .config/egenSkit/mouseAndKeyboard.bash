#!/bin/bash

# Setting up scrolling on laptop
if synclient &>/dev/null; then
  synclient HorizTwoFingerScroll=1
  synclient HorizScrollDelta=250
  synclient PalmDetect=1
  synclient VertEdgeScroll=0
  synclient TapButton2=2
  synclient PalmMinWidth=8

  # When on a ThinkPad
  if xinput list | grep -E "TrackPoint"; then
    deviceNumber=$(xinput list | grep -E "TrackPoint" | sed -rn 's/.*id=([0-9]+).*/\1/p')
    xinput set-prop $deviceNumber "Device Accel Profile" 6
    xinput set-prop $deviceNumber "Device Accel Constant Deceleration" 2
    xinput set-prop $deviceNumber "Device Accel Adaptive Deceleration" 1
    xinput set-prop $deviceNumber "Device Accel Velocity Scaling" 200.0
    xinput set-prop $deviceNumber "Evdev Wheel Emulation Inertia" 5
  fi
  # When on a Dell Latitude
  if xinput list | grep -E "DualPoint Stick"; then
    deviceNumber=$(xinput list | grep -E "DualPoint Stick" | sed -rn 's/.*id=([0-9]+).*/\1/p')
    xinput set-prop $deviceNumber "Device Accel Profile" 2
    xinput set-prop $deviceNumber "Device Accel Constant Deceleration" 1.0
    xinput set-prop $deviceNumber "Device Accel Adaptive Deceleration" 10.0
    xinput set-prop $deviceNumber "Device Accel Velocity Scaling" 2.0
    xinput set-prop $deviceNumber "Evdev Wheel Emulation Inertia" 30
    xinput set-prop $deviceNumber "Coordinate Transformation Matrix" 1.000, 0.000, 0.000, 0.000, 1.000, 0.000, 0.000, 0.000, 1.200
  fi


fi
# Set the keymap and set caps to be a windows key.
setxkbmap qq dvorak
setxkbmap -option caps:super
