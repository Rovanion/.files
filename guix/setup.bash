#!/usr/bin/env bash

scriptdir=$(dirname -- $(readlink -e "$0"))
cd $scriptdir

packages=$(../package-lists.sh $1)

declare -rA guix_package_translations=(
	[maildir-utils]=mu
	[mu4e]=mu
	[aspell-sv]=aspell-dict-sv
	[aspell-en]=aspell-dict-en
	[bind9-host]=knot:tools
	[augeas-tools]=augeas
	[emacs-nox]=emacs-no-x
	[fonts-terminus]=font-terminus
	[x11-utils]='xfontsel setxkbmap'
	[spotify]=spotifyd  	# Technically not a spotify client, instead something a spotify client can connect to.
	[emacs-gtk]=emacs
	[ssh-askpass-fullscreen]=x11-ssh-askpass
	[network-manager-gnome]=network-manager-applet
	[ykcs11]=yubico-piv-tool
	[volumeicon-alsa]=volumeicon
	[python3]=python
	[ncurses-bin]=ncurses
	[openssh-client]=openssh-sans-x
)
guix_packages=()
for package in ${packages[@]}; do
	guix_packages+=(${guix_package_translations[$package]:-$package})
done


### Package installation

guix pull
guix install ${guix_packages[@]}


### Run related setup scripts

# Lastly symlink in all the config if not already done
if [[ ! -f "$HOME/.gitconfig" ]]; then
	./symlinker.sh
fi

# Run mailconf setup script.
../mailconf/setup.sh
