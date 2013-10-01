#!/usr/bin/sh

# symlinker.sh
# create symbolic link of dotfiles in which same with this script in your home directory
# NOTICE: this script processes only files whose name begin with . (dot)

echo -e "creating symbolic link of dotfiles..."

shdir="$(cd $(dirname $0) && pwd)"

cd ${shdir}

for filepath in ${shdir}/.*
do
    if [ \( -f $filepath -o -d $filepath \) -a $filepath != "${shdir}/." -a $filepath != "${shdir}/.." -a $filepath != "${shdir}/.git" ] ; then
	echo "creating link: ${filepath} -> ${HOME}"
	ln -s ${filepath} ${HOME}
    fi
done
