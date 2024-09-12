#!/bin/bash -e

# Keep track of the last executed command.
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# Print an error message before exiting.
trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

scriptdir=$(dirname -- $(readlink -e "$0"))
cd $scriptdir
gitroot=$(git rev-parse --show-toplevel)

command -v git &>/dev/null || { echo 'Git not found, install it first.'; exit 1; }

git submodule init
git submodule update


### Functions

function install-signal-spotify-repos {
	# 1. Install Signal's official public software signing key:
	wget -O- https://updates.signal.org/desktop/apt/keys.asc | gpg --dearmor > signal-desktop-keyring.gpg
	cat signal-desktop-keyring.gpg | sudo tee /usr/share/keyrings/signal-desktop-keyring.gpg > /dev/null
	# 2. Add Signal's repository to your list of repositories:
	sudo bash -c 'echo "deb [arch=amd64 signed-by=/usr/share/keyrings/signal-desktop-keyring.gpg] https://updates.signal.org/desktop/apt xenial main" > /etc/apt/sources.list.d/signal-xenial.list'
	## And Spotify
	curl -sS https://download.spotify.com/debian/pubkey_6224F9941A8AA6D1.gpg | \
		sudo gpg --dearmor --yes -o /etc/apt/trusted.gpg.d/spotify.gpg
	sudo bash -c 'echo "deb http://repository.spotify.com stable non-free" > /etc/apt/sources.list.d/spotify.list'
}


### Package lists.
source package-lists.bash


### Distribution specific code paths.

distributor=$(lsb_release --id --short 2>/dev/null || cat /etc/issue | tail -n 1)
case $distributor in
	Debian)
		firefox_name=firefox-esr
		codec_packages=(gstreamer1.0-libav gstreamer1.0-plugins-ugly gstreamer1.0-vaapi unrar)
		if ! grep -q contrib /etc/apt/sources.list; then
			read -p "Contrib not found in /etc/apt/sources.list, do you want to enable it along with non-free and non-free-firmware? [Yn]" yesno
			if [[ ! $yesno =~ "[Nn]*" ]]; then
					sudo apt install software-properties-common
					sudo add-apt-repository --yes contrib non-free non-free-firmware
			fi
		fi
		# Debian's LightDM does not come with Xsession integration, for some reason.
		# https://wiki.debian.org/LightDM#User_configuration
		# Install the script taken from the Ubuntu 24.04 package.
		sudo cp --no-clobber "$gitroot/lightdm-session" /usr/sbin/lightdm-session
		;;
	Ubuntu)
		firefox_name=firefox
		codec_packages=(ubuntu-restricted-extras)
		;;
	'This is the GNU system.  Welcome.')
		# This is the insane way Guix identifies itself.
		firefox_name=firefox
		codec_packages=()

		exit 1
		;;
esac


### Computer usage specific code paths.

case $1 in
	workstation|leisure)
		packages=(${base_packages[@]} ${graphical_workstation_packages[@]} ${graphical_packages[@]} ${codec_packages[@]} $firefox_name)
		systemctl --user enable ssh-agent
		[ -f /etc/apt/sources.list.d/spotify.list ] || install-signal-spotify-repos
		# Make nautilus not search through all files when you type anything
		if ! gsettings set org.gnome.nautilus.preferences enable-interactive-search true; then
			echo "The installed version of Nautilus does not support non-recursive search."
		fi
		# Prevent suspend on laptop lid closure.
		if ! grep -q 'HandleLidSwitch=ignore' /etc/systemd/logind.conf; then
			cat <<-EOF | sudo tee -a /etc/systemd/logind.conf >/dev/null
				HandleLidSwitch=ignore
				HandleLidSwitchDocked=ignore
				HandleLidSwitchExternalPower=ignore
			EOF
			sudo systemctl restart systemd-logind.service
		fi

		# Set the gtk controls to behave like emacs
		gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"
		# Make sure that the screen shot directory exists
		mkdir -p ~/Pictures/scrot
		## Select default X tools.
		sudo update-alternatives --set x-terminal-emulator /usr/bin/urxvt
		sudo update-alternatives --set x-www-browser /usr/bin/${firefox_name}
		xdg-settings set default-web-browser ${firefox_name}.desktop
		;;& # Resume matching to pick up workstation-only rule.
	workstation)
		systemctl --user enable pomodoro ;;
	leisure) ;;
	headless-workstation) ;;
	server) ;;
	*)
		echo "First argument should be one of workstation, leisure, headless-workstation or server."
		exit 2 ;;
esac

case $1 in
	workstation|leisure)
		# Allow me to run light as root without password.
		sudo bash -c 'echo "rovanion ALL=(ALL) NOPASSWD: /usr/bin/light" > /etc/sudoers.d/rovanion'
		# Configure LightDM.
		mkdir -p /etc/lightdm/lightdm.conf.d
		if ! [ -L /etc/lightdm/lightdm.conf.d/01_rovanion.conf ]; then
			sudo ln -s /etc/lightdm/lightdm.conf.d/01_rovanion.conf "$gitroot/lightdm.conf"
		fi
		# Install my own keymap.
		if ! [ -L /usr/share/X11/xkb/symbols/qq ]; then
			sudo ln -s /usr/share/X11/xkb/symbols/qq "$gitroot/.config/xkb/symbols/qq"
		fi

	;;
esac


### Install packages.

sudo apt update
sudo apt install "${packages[@]}"


### Generic setup

sudo update-alternatives --set editor /usr/bin/emacs
# Really ignore changes to htoprc, even though we carry a baseline conf in the repo
git update-index --assume-unchanged .config/htop/htoprc



### Run related setup scripts

# Lastly symlink in all the config if not already done
if [[ ! -f "$HOME/.gitconfig" ]]; then
	./symlinker.sh
fi

# Run mailconf setup script.
./mailconf/setup.sh


### Yubikey hardware action
cp ./yubikey-ssh-agent-attached.bash /usr/local/bin/yubikey-ssh-agent-attached
cp ./yubikey-ssh-agent.rules         /etc/udev/rules.d/yubikey-ssh-agent.rules

# Clear traps.
trap - DEBUG
trap - EXIT
