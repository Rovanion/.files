#!/bin/sh -eu
# The main export of this file is the variable $packages, written to stdout.


## Ensure the first argument was given.
if [ -z ${1+x} ]; then
	 echo "$0: This command takes one or two positional arguments, 0 were given."
	 exit 2
fi
role=$1


### Distribution specific code paths.

distributor=$(lsb_release --id --short 2>/dev/null || cat /etc/issue | tail -n 1)
case $distributor in
	Debian)
		codec_packages=(gstreamer1.0-libav gstreamer1.0-plugins-ugly gstreamer1.0-vaapi unrar)
		;;
	Ubuntu)
		codec_packages=(ubuntu-restricted-extras)
		;;
	'This is the GNU system.  Welcome.')
		# The above is the insane way Guix identifies itself.
		codec_packages=()
		;;
	*)
		echo "$0: This script \"only\" supports Debian, Ubuntu and Guix."
		exit 3
		;;
esac


### Package lists.

base_packages=(
	locales                       # Localization and translation.
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
	ncdu                          # Disk usage in a TUI.
	less                          # I've never had to manually install a pager before, but here we are.
	ripgrep                       # Ripgrep is a fast grep replacement.
)

headless_packages=(
	emacs-nox                     # Emacs, duh.
)

graphical_workstation_packages=(
	ykcs11                        # Yubikey SSH integration.
	yubico-piv-tool               # Yubikey manipulation tools.
)

graphical_packages=(
	lightdm                       # Display manager.
	awesome                       # Window manager.
	firefox                       # Web browser.
	keepassxc                     # Password manager.
	nsxiv                         # Image viewer.
	dolphin                       # Filesystem explorer.
	network-manager-gnome         # nm-applet
	cups                          # Printing.
	ssh-askpass-fullscreen        # Graphical "OK to use yubikey?" or "OK to use shared SSH connection?".
	evince                        # PDF viewer.
	conky                         # Clock on desktop that I never see.
	audacious                     # Music player that I never use.
	mpv                           # Movie player that I actually use.
	redshift                      # Tint screen red when the sun goes down.
	blueman                       # Bluetooth, blueman-applet.
	fluidsynth                    # MIDI sound font.
	qgis                          # Always end up editing maps in some way.
	emacs-gtk                     # For x-get-resource function.
	signal-desktop                # Signal chat.
	spotify                       # Music yao.
	alsa-utils                    # alsamixer
	pulsemixer                    # TUI Pulse Audio mixer. Hopefully superseeded by Pipewire soon.
	pavucontrol                   # The GUI pulse audio mixer.
	x11-utils                     # To install xfontsel, the font viewer.
	mumble                        # The VoIP client.
	volumeicon-alsa               # Tray icon for adjusting the volume.
	rxvt-unicode                  # Terminalemulator ju!
	file-roller                   # Archive manager.
	audacity                      # Sound editor.
	vlc                           # Media player.
	xkbcomp                       # Dependency of ,configure-mouse-and-keyboard.
	libreoffice                   # Office suite.
	physlock                      # Screen lock.
	light                         # Control the screen brightness of laptop displays.
	fonts-wqy-microhei            # Chinese sans-serif font.
	hicolor-icon-theme            # Base icons used by NetworkManager.
	arandr                        # Dynamic display management.
	xrandr                        # Used in scripts ,screenlayout-*.
	thunar                        # File manager.
	scrot                         # Screenshots taker. Bound to PrtSc in Awesome.
	xsel                          # Manipulation of Xorg selection and copy buffers. Copy/Paste.
)


case $role in
	workstation|leisure)
		packages=(${base_packages[@]} ${graphical_workstation_packages[@]} ${graphical_packages[@]} ${codec_packages[@]}) ;;
	headless-workstation)
		packages=(${base_packages[@]} ${workstation_packages[@]} ${headless_packages[@]}) ;;
	server)
		packages=(${base_packages[@]} ${headless_packages[@]}) ;;
	*)
		echo "$0: First argument should be one of workstation, leisure, headless-workstation or server."
		exit 2 ;;
esac



### Distribution specific package translation

case $distributor in
	'This is the GNU system.  Welcome.')
		# The above is the absolutely insane way Guix identifies itself.
		declare -rA package_translations=(
			[maildir-utils]=mu
			[mu4e]=mu
			[aspell-sv]=aspell-dict-sv
			[aspell-en]=aspell-dict-en
			[bind9-host]=knot:tools
			[augeas-tools]=augeas
			[emacs-nox]=emacs-no-x
			[fonts-terminus]=font-terminus
			[fonts-wqy-microhei]=font-wqy-microhei
			[x11-utils]='xfontsel setxkbmap'
			[spotify]='hello' # spotifyd did not build last rebuild. # Technically not a spotify client, instead something a spotify client can connect to.
			[emacs-gtk]=emacs
			[ssh-askpass-fullscreen]=x11-ssh-askpass
			[network-manager-gnome]=network-manager-applet
			[ykcs11]=yubico-piv-tool
			[volumeicon-alsa]=volumeicon
			[python3]=python
			[ncurses-bin]=ncurses
			[openssh-client]=openssh-sans-x
			[locales]=glibc-locales                          # All locales, could be replaced with i.e. ((@ (gnu packages base) make-glibc-utf8-locales) (@ (gnu packages base) glibc) #:locales (list "en_US" "sv_SE") #:name "glibc-utf8-locales-en-se")
		)
		;;
	'Debian')
		declare -rA package_translations=(
			[firefox]=firefox-esr
		)
		;;
esac

translated_packages=()
for package in ${packages[@]}; do
	translated_packages+=(${package_translations[$package]:-$package})
done
packages=()
packages=${translated_packages[@]}

# The final output of this script the program is printed here.
echo ${packages[@]}
