#!/bin/sh

# Bail on failure.
set -u

###
# Script which symlinks the contents of this git project into $HOME.
###

git_root=$(git rev-parse --show-toplevel)

dest="$HOME"
if [ -n "${1+x}" ]; then
    dest=$1
    echo "Explicit destination $1 given."
fi
echo "Creating links from $git_dir in $dest"

for file in $(ls -a); do
	# If the file is ., .., .git, this script or README then do nothing.
	if echo $file | grep -Eq '^\.$|^\.\.$|^\.git$|^\.ssh$|^.+\.bash~?$|^.+\.sh~?$|^.+\.rules~?$|^.+\.conf~?$|^README$|^mailconf$|^guix$|^lightdm-session$'; then
		continue
	fi

	if [ -e $dest/$file ]; then
		echo $file already exists, moving existing to $file.old
		mv $dest/$file $dest/$file.old
	fi
	ln -s $git_dir/$file $dest/$file
done

# Special case for .ssh since some systems (RHEL) does not allow for ~/.ssh to be a symlink.
chmod g-rw,o-r $git_dir/.ssh/config
ln $git_dir/.ssh/config $dest/.ssh/config
