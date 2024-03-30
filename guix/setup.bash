#!/usr/bin/env bash
source ../package-lists.bash

declare -rA guix_package_translations=(
	[maildir-utils]=mu
	[mu4e]=mu
	[aspell-sv]=aspell-dict-sv
	[aspell-en]=aspell-dict-en
	[bind9-host]=knot:tools
	[augeas-tools]=augeas
	[emacs-nox]=emacs-no-x
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
	./symlinker.bash
fi

# Run mailconf setup script.
./mailconf/setup.sh
