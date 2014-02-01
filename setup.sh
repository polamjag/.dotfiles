#!/bin/sh
# setup.sh - setting up dotfiles

echo -e "Creating symbolic link of dotfiles ..."

shdir="$(cd $(dirname $0) && pwd)"

cd ${shdir}

# common files
function setup_dotfiles() {
    for filepath in ${shdir}/.* ; do
        if [ \( -f $filepath -o -d $filepath \) -a $filepath != "${shdir}/." -a $filepath != "${shdir}/.." -a $filepath != "${shdir}/.git" -a $filepath != "${shdir}/.gitconfig" ] ; then
            echo "creating link: ${filepath} -> ${HOME}"
            ln -s ${filepath} ${HOME}
        fi
    done
}
echo -n "Setup dotfiles? (y/n) "
read $answer
case $answer in
    y)
        setup_dotfiles
        ;;
    Y)
        setup_dotfiles
        ;;
    yes)
        setup_dotfiles
        ;;
    *)
        echo
        ;;
esac

# .gitconfig
function setup_gitconfig() {
    ln -s ${shdir}/.gitconfig $HOME
}
if [ ! -e $HOME/.gitconfig ] ; then
    echo -n "Use .gitconfig? (y/n) "
		read $answer
    case $answer in
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

 .emacs.d
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
    for filepath in ${shdir}/bin/* ; do
        echo "creating link: ${filepath} -> ${HOME}/bin/"
        ln -s ${filepath} ${HOME}/bin/
    done
}
echo -n "Copy shell scripts **for console** into ~/bin? (y/n) "
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
        echo 
        ;;
esac

# ~/bin; my shell scripts
setup_shellscripts_x_to_bin () {
    if [ ! -d ${HOME}/bin ] ; then
        mkdir ${HOME}/bin
    fi
    for filepath in ${shdir}/bin_x/* ; do
        echo "creating link: ${filepath} -> ${HOME}/bin/"
        ln -s ${filepath} ${HOME}/bin/
    done
}
echo -n "Copy shell scripts **for X Desktop Environment** into ~/bin? (y/n) "
read answer
case $answer in
    y)
        setup_shellscripts_x_to_bin
        ;;
    Y)
        setup_shellscripts_x_to_bin
        ;;
    yes)
        setup_shellscripts_x_to_bin
        ;;
    *)
        echo
        ;;
esac

echo "Finished setup"

