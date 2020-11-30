local awful = require("awful")
local utils = require("utils")


-- {{{ Autostart
--Autostart scripting

utils.run_if_not_running("configure-mouse-and-keyboard")
utils.run_if_not_running("firefox")
utils.run_if_not_running("volumeicon")
utils.run_if_not_running("nm-applet")
utils.run_if_not_running("audacious", "-m")
utils.run_if_not_running("emacs", "--daemon");
utils.run_if_not_running("urxvt", "-e mosh --ssh='ssh -p 9001' rovanion.se -- screen -rd weechat")
utils.run_if_not_running("conky")
utils.run_if_not_running("redshift", "-m vidmode -l 50:16")
utils.run_if_not_running("dropbox start")
--}}}
