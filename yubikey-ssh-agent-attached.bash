#!/bin/bash
$log=/var/log/yubi-ssh-agent-attached
echo $(date) $UID $@ >>$log

if [[ $UID == 0 ]]; then
	chown rovanion $log
	echo Switching to user rovanion, running $0 with args $@ >>$log
	exec su rovanion - $0 $@
fi

export XAUTHORITY=~/.Xauthority
export DISPLAY=:0
export SSH_AUTH_SOCK=/run/user/$UID/ssh-agent.socket

if [[ $1 == "add" ]]; then
	urxvt -e ~/.local/bin/yubi-add &
elif [[ $1 == "remove" ]]; then
	ssh-add -d -s /usr/lib/x86_64-linux-gnu/libykcs11.so
fi
