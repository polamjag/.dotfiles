#!/bin/sh

# setup.sh - setting up dotfiles
# create symbolic link of dotfiles in which same with this script in your home directory
# NOTICE: this script processes only files whose name begin with . (dot)

echo -e "Creating symbolic link of dotfiles ..."

shdir="$(cd $(dirname $0) && pwd)"

cd ${shdir}

for filepath in ${shdir}/.*
do
    if [ \( -f $filepath -o -d $filepath \) -a $filepath != "${shdir}/." -a $filepath != "${shdir}/.." -a $filepath != "${shdir}/.git" -a $filepayh != "${shdir}/.gitconfig" ] ; then
				echo "creating link: ${filepath} -> ${HOME}"
				ln -s ${filepath} ${HOME}
    fi
done

  
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


function setup_emacsd() {
		echo "Cloning .emacs.d ..."
		cd $HOME
		git clone https://github.com/polamjag/.emacs.d.git
		pwd
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

echo "Finished setup"
