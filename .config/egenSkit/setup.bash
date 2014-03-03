#!/bin/bash

# Install my own keymap
sudo mv ~/.config/egenSkit/qq /usr/share/X11/xkb/symbols/

# Make nautilus not search through all files when you type anything
gsettings set org.gnome.nautilus.preferences enable-interactive-search true


# Debian dependency list:
sudo apt-get install awesome feh conky firefox emacs audacious nautilus hunspell eog redshift


# In order to add emerge as mergetool run:
git config --global merge.tool emerge
git config --global mergetool.emerge.path $HOME/.emacs.d/emerge-for-git

# Fancy pants git log
git config --global alias.lg "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit"
