#!/bin/bash

if [[ ! $1 == nox ]]; then
	# Install my own keymap
	sudo cp ~/.config/xkb/symbols/qq /usr/share/X11/xkb/symbols/

	# Make nautilus not search through all files when you type anything
	gsettings set org.gnome.nautilus.preferences enable-interactive-search true

	# Set the gtk controls to behave like emacs
	gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"
	gconftool-2 --type=string --set /desktop/gnome/interface/gtk_key_theme Emacs

	# Make sure that the screen shot directory exists
	mkdir -p ~/Pictures/scrot

	# Debian dependency list:
	sudo apt-get install git awesome feh conky firefox emacs audacious nautilus hunspell eog redshift htop ttf-mscorefonts-installer xfonts-terminus xfonts-terminus-dos rxvt-unicode volumeicon-alsa file-roller keepassx mu4e maildir-utils weechat aspell-sv aspell-en glances mosh global apt-file openjdk-8-jdk chromium-browser pavucontrol thunar xsel rxvt-unicode emacs ipython3 virtualenv python-pip scrot tmux physlock
	if lsb_release -i | grep -q Ubuntu; then
		sudo apt-get install ubuntu-restricted-extras
	else
		sudo apt-get install gstreamer0.10-plugins-good gstreamer0.10-plugins-bad gstreamer1.0-plugins-ugly
	fi

	# Select default x tools on debian
	sudo update-alternatives --set x-terminal-emulator /usr/bin/urxvt
	sudo update-alternatives --set x-www-browser /usr/bin/firefox
else
	sudo apt-get install git emacs-nox htop screen maildir-utils mu4e weechat aspell-sv aspell-en glances tmux
fi

# Fix GNU ELPA GPG keys being out of date in Ubuntu 18.04
if lsb_release --id | grep -q Ubuntu && lsb_release --release | grep -q 18.04; then
	gpg --homedir ~/.emacs.d/elpa/gnupg/ --receive-keys 066DAFCB81E42C40
fi

# Fancy pants git log
git config --global alias.lg "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit"

# Additional setup for git
git config --global user.name "Rovanion Luckey"
git config --global user.email "rovanion.luckey@gmail.com"
git config --global credential.helper cache
git config --global credential.helper 'cache --timeout=3600'
git config --global push.default simple
git config --global core.editor 'emacsclient -t -a=\"\"'

# Set emacs as the default editor
sudo update-alternatives --set editor /usr/bin/emacs25

# Lastly symlink in all the config
./symlinker.bash

# Really ignore changes to htoprc, even though we carry a baseline conf in the repo
git update-index --assume-unchanged .config/htop/htoprc
