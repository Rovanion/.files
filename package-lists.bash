#!/bin/bash

base_packages=(
	htop
	screen
	tmux
	## Emacs as mail client.
	maildir-utils								 	# mu
	isync													# mbsync
	mu4e													# Emacs mode.
	weechat												# Chat.
	aspell-sv
	aspell-en
	hunspell
	direnv												# Start directory specific environment on cd.
	clojure												# Also brings in the JVM.
	rlwrap												# Add GNU readline to any command.
	bind9-host                    # host
	augeas-tools									# augtool, configuration editing.
	shellcheck										# Check bash scripts for errors.
	ncurses                       # tput, determines terminal capabilities.
)

headless_packages=(
	emacs-nox											# Emacs, duh.
)

graphical_workstation_packages=(
	ykcs11												# Yubikey SSH integration.
	yubico-piv-tool								# Yubikey manipulation tools.
	alsa-utils										# alsamixer
	pulsemixer										# TUI Pulse Audio mixer. Hopefully superseeded by Pipewire soon.
)

graphical_packages=(
	lightdm												# Display manager.
	awesome												# Window manager.
	keepassxc											# Password manager.
	nsxiv													# Image viewer.
	dolphin												# Filesystem explorer.
	network-manager-gnome					# nm-applet
	cups													# Printing.
	ssh-askpass-gnome							# Graphical "OK to use yubikey?".
	evince												# PDF viewer.
	conky													# Clock on desktop that I never see.
	audacious											# Music player that I never use.
	mpv														# Movie player that I actually use.
	redshift											# Tint screen red when the sun goes down.
	blueman												# Bluetooth, blueman-applet.
	fluidsynth										# MIDI sound font.
	qgis													# Always end up editing maps in some way.
	emacs-gtk											# For x-get-resource function.
	signal-desktop								# Signal chat.
	spotify												# Music yao.
)
