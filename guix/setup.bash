#!/usr/bin/env bash

scriptdir=$(dirname -- $(readlink -e "$0"))
cd $scriptdir

packages=$(../package-lists.sh $1)

### Package installation

echo "Installing packages: ${packages}"
guix pull
guix install ${packages[@]}


### Run related setup scripts

# Lastly symlink in all the config if not already done
if [[ ! -f "$HOME/.gitconfig" ]]; then
	./../symlinker.sh
fi

# Run mailconf setup script.
../mailconf/setup.sh
