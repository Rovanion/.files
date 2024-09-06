#!/usr/bin/env bash

scriptdir=$(dirname -- $(readlink -e "$0"))
cd $scriptdir

source ../package-lists.bash

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
)
packages=()
for package in ${base_packages[@]}; do
	packages+=(${guix_package_translations[$package]:-$package})
done


### Package installation

guix pull
guix install ${packages[@]}


### Run related setup scripts

# Lastly symlink in all the config if not already done
if [[ ! -f "$HOME/.gitconfig" ]]; then
	./symlinker.sh
fi

# Run mailconf setup script.
../mailconf/setup.sh
