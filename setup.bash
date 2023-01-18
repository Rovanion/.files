#!/bin/bash -e

# Keep track of the last executed command.
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# Print an error message before exiting.
trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

scriptdir=$(dirname -- $(readlink -e "$0"))
cd $scriptdir
gitroot=$(git rev-parse --show-toplevel)

git submodule init
git submodule update

if [[ ! $1 == nox ]]; then
	# Install my own keymap
	sudo cp "$gitroot/.config/xkb/symbols/qq" /usr/share/X11/xkb/symbols/

	# Make nautilus not search through all files when you type anything
	if ! gsettings set org.gnome.nautilus.preferences enable-interactive-search true; then
	    echo "The installed version of Nautilus does not support non-recursive search."
	fi

	# Set the gtk controls to behave like emacs
	gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"

	# Make sure that the screen shot directory exists
	mkdir -p ~/Pictures/scrot

	# Debian dependency list:
	sudo apt-get install git awesome feh conky firefox emacs audacious nautilus hunspell eog redshift htop ttf-mscorefonts-installer xfonts-terminus xfonts-terminus-dos rxvt-unicode volumeicon-alsa file-roller keepassx mu4e maildir-utils weechat aspell-sv aspell-en mosh global apt-file openjdk-8-jdk chromium-browser pavucontrol thunar xsel rxvt-unicode emacs ipython3 virtualenv python3-pip scrot tmux physlock direnv syncthing light arandr isync lm-sensors keepassxc
	if lsb_release -i | grep -q -E "Ubuntu|Linuxmint"; then
		sudo apt-get install ubuntu-restricted-extras
	else
		sudo apt-get install gstreamer0.10-plugins-good gstreamer0.10-plugins-bad gstreamer1.0-plugins-ugly
	fi

	# Select default x tools on debian
	sudo update-alternatives --set x-terminal-emulator /usr/bin/urxvt
	sudo update-alternatives --set x-www-browser /usr/bin/firefox
else
	sudo apt-get install git emacs-nox htop screen maildir-utils mu4e weechat aspell-sv aspell-en tmux direnv
fi

# Fix GNU ELPA GPG keys being out of date in Ubuntu 18.04
if lsb_release --id | grep -q Ubuntu && lsb_release --release | grep -q 18.04; then
	gpg --homedir ~/.emacs.d/elpa/gnupg/ --receive-keys 066DAFCB81E42C40
fi

# Set emacs as the default editor
sudo update-alternatives --set editor /usr/bin/emacs

# Lastly symlink in all the config if not already done
if [[ ! -f "$HOME/.gitconfig" ]]; then
	./symlinker.bash
fi

# Really ignore changes to htoprc, even though we carry a baseline conf in the repo
git update-index --assume-unchanged .config/htop/htoprc


# Clear traps.
trap - DEBUG
trap - EXIT
