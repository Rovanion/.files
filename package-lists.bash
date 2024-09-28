#!/bin/bash -eu

# The main export of this file is the variable $packages.

### Distribution specific code paths.

distributor=$(lsb_release --id --short 2>/dev/null || cat /etc/issue | tail -n 1)
case $distributor in
	Debian)
		firefox_name=firefox-esr
		codec_packages=(gstreamer1.0-libav gstreamer1.0-plugins-ugly gstreamer1.0-vaapi unrar)
		;;
	Ubuntu)
		firefox_name=firefox
		codec_packages=(ubuntu-restricted-extras)
		;;
	'This is the GNU system.  Welcome.')
		# This is the insane way Guix identifies itself.
		firefox_name=firefox
		codec_packages=()
		;;
esac


base_packages=(
	htop
	screen
	tmux
	## Emacs as mail client.
	maildir-utils                 # mu
	isync                         # mbsync
	mu4e                          # Emacs mode.
	weechat                       # Chat.
	## Emacs recommended
	unzip                         # Unzips compressed archives.
	zip                           # Makes compressed archives.
	aspell-sv
	aspell-en
	hunspell
	direnv                        # Start directory specific environment on cd.
	clojure                       # Also brings in the JVM.
	rlwrap                        # Add GNU readline to any command.
	bind9-host                    # host
	augeas-tools                  # augtool, configuration editing.
	shellcheck                    # Check bash scripts for errors.
	ncurses-bin                   # tput, determines terminal capabilities.
	rsync                         # To copy files.
	fonts-terminus                # Terminal monospace font.
	syncthing                     # The decentralized Dropbox.
	openssh                       # The secure shell client.
	recutils                      # recsel, search through text output.
	python3
	perl                          # For diff-highlight.
)

headless_packages=(
	emacs-nox											# Emacs, duh.
)

graphical_workstation_packages=(
	ykcs11												# Yubikey SSH integration.
	yubico-piv-tool								# Yubikey manipulation tools.
)

graphical_packages=(
	lightdm												# Display manager.
	awesome												# Window manager.
	keepassxc											# Password manager.
	nsxiv													# Image viewer.
	dolphin												# Filesystem explorer.
	network-manager-gnome					# nm-applet
	cups													# Printing.
	ssh-askpass-fullscreen        # Graphical "OK to use yubikey?" or "OK to use shared SSH connection?".
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
	alsa-utils										# alsamixer
	pulsemixer										# TUI Pulse Audio mixer. Hopefully superseeded by Pipewire soon.
	x11-utils										# To install xfontsel, the font viewer.
	mumble 										  # The VoIP client.
	volumeicon-alsa							# Tray icon for adjusting the volume.
	rxvt-unicode                  # Terminalemulator ju!
	file-roller                   # Archive manager.
	audacity                      # Sound editor.
	vlc                           # Media player.
	xkbcomp                       # Dependency of configure-mouse-and-keyboard.
	libreoffice                   # Office suite.
	physlock                      # Screen lock.
)

case $1 in
	workstation|leisure)
		packages=(${base_packages[@]} ${graphical_workstation_packages[@]} ${graphical_packages[@]} ${codec_packages[@]} $firefox_name) ;;
	headless-workstation)
		packages=(${base_packages[@]} ${workstation_packages[@]} ${headless_packages[@]}) ;;
	server)
		packages=(${base_packages[@]} ${headless_packages[@]}) ;;
	*)
		echo "$0: First argument should be one of workstation, leisure, headless-workstation or server."
		exit 2 ;;
esac
