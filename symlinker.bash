#!/usr/bin/env bash

###
# Script which symlinks the contents of it's folder into the parent folder.
###

currentDir=$(pwd)
dest="$HOME"
if [[ $1 != "" ]]; then
	dest=$1
fi
echo "Creating links in $dest"

for file in $(ls -a); do
	# If the file is ., .., .git, etc. this script or README then do nothing.
	if [[ $file =~ ^.$|^..$|^.git$|^.ssh$|^.+.bash~?$|^README|^mailconf$|^guix$ ]]; then
		continue
	fi

	if [[ -e $dest/$file ]]; then
		echo $file already exists, moving existing to $file.old
		mv $dest/$file $dest/$file.old
	fi
	ln -s $currentDir/$file $dest/$file
done

# Special case for .ssh since some systems (RHEL) does not allow for ~/.ssh to be a symlink.
chmod g-rw,o-r $currentDir/.ssh/config
ln $currentDir/.ssh/config $dest/.ssh/config
