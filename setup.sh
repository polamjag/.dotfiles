#!/bin/sh
# setup.sh - setting up dotfiles

echo -e "Creating symbolic link of dotfiles ..."

shdir="$(cd $(dirname $0) && pwd)"

cd ${shdir}

# common files
for filepath in ${shdir}/.* ; do
    if [ \( -f $filepath -o -d $filepath \) -a $filepath != "${shdir}/." -a $filepath != "${shdir}/.." -a $filepath != "${shdir}/.git" -a $filepath != "${shdir}/.gitconfig" ] ; then
				echo "creating link: ${filepath} -> ${HOME}"
				ln -s ${filepath} ${HOME}
    fi
done

# .gitconfig
function setup_gitconfig() {
    ln -s .gitconfig $HOME
}
if [ ! -e $HOME/.gitconfig ] ; then
    echo "Use .gitconfig? (y/n)"
    case read in
        y)
            setup_gitconfig
            ;;
        Y)
            setup_gitconfig
            ;;
        yes)
            setup_gitconfig
            ;;
        *)
            ;;
    esac
fi

# .emacs.d
function setup_emacsd() {
		echo "Cloning .emacs.d ..."
		cd $HOME
		git clone https://github.com/polamjag/.emacs.d.git
}
if [ ! -e ~/.emacs.d ] ; then
		echo -n "Would you setup .emacs.d with github.com:polamjag/.emacs.d.git? (y/n) "
		read answer
		case $answer in
				y)
						setup_emacsd
						;;
				Y)
						setup_emacsd
						;;
				yes)
						setup_emacsd
						;;
				*)
						echo "Aborted cloning .emacs.d"
						;;
		esac
fi

# ~/bin; my shell scripts
setup_shellscripts_to_bin () {
		if [ ! -d ${HOME}/bin ] ; then
				mkdir ${HOME}/bin
		fi
		for filepath in ${shdir}/scripts/* ; do
				echo "creating link: ${filepath} -> ${HOME}/bin/"
				ln -s ${filepath} ${HOME}/bin/
		done
}
echo -n "Copy shell scripts into ~/bin? (y/n) "
read answer
case $answer in
		y)
				setup_shellscripts_to_bin
				;;
		Y)
				setup_shellscripts_to_bin
				;;
		yes)
				setup_shellscripts_to_bin
				;;
		*)
				echo "Aborted copying shell scripts"
				;;
esac

echo "Finished setup"
