# Load our actual bash rc.
. ~/.bashrc
if [ -e /home/rovanion/.nix-profile/etc/profile.d/nix.sh ]; then . /home/rovanion/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
