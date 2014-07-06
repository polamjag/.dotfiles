#!/bin/bash

shdir="$(cd $(dirname $0) && pwd)"
cd ${shdir}

setup_dot() {
    echo "  Setting up dotfiles ..."
    for filepath in ${shdir}/.* ; do
        if [ \( -f $filepath -o -d $filepath \) -a $filepath != "${shdir}/." -a $filepath != "${shdir}/.." -a $filepath != "${shdir}/.git" -a $filepath != "${shdir}/.gitconfig" -a $filepath != "${shdir}/.gitignore" -a $filepath != "${shdir}/.gitmodules"  -a $filepath != "${shdir}/.zshenv.exam" ] ; then
            echo "    Creating link: ${filepath} -> ${HOME}"
            ln -s ${filepath} ${HOME}
        fi
    done
}
setup_git() {
    echo "  Setting up .gitconfig ..."
    ln -s ${shdir}/.gitconfig $HOME
}
setup_bin () {
    echo "  Setting up ~/bin ..."
    if [ ! -d ${HOME}/bin ] ; then
        mkdir ${HOME}/bin
    fi
    for filepath in ${shdir}/bin/* ; do
        echo "    Creating link: ${filepath} -> ${HOME}/bin/"
        ln -s ${filepath} ${HOME}/bin/
    done
}
setup_binx () {
    echo "  Setting up ~/bin_x ..."
    if [ ! -d ${HOME}/bin ] ; then
        mkdir ${HOME}/bin
    fi
    for filepath in ${shdir}/bin_x/* ; do
        echo "    Creating link: ${filepath} -> ${HOME}/bin/"
        ln -s ${filepath} ${HOME}/bin/
    done
}
setup_vim () {
    echo "  Setting up ~/.vim/ ..."
    cd $shdir
    git submodule init
    git submodule update
    cd $shdir/.vim/vimproc
    make
}

if [ $# -eq 1 -a "$1" = "--usage" ] ; then
    echo "$0 [--usage] [--force <args>]"
    echo "args: dot, git, bin, binx, vim"
    echo
    echo "e.g.: \`$0 --force dot git\`"
    exit 0
fi

if [ $# -gt 1 -a "$1" = "--force" ] ; then
    echo "Running in force mode"
    shift
    while [ $# -gt 0 ] ; do
        setup_$1
        shift
    done
    exit 0
fi
echo -e "Creating symbolic link of dotfiles ..."


# common files
echo -n "Setup dotfiles? (y/n) "
read answer
case $answer in
    y)
        setup_dot
        ;;
    Y)
        setup_dot
        ;;
    yes)
        setup_dot
        ;;
    *)
        echo
        ;;
esac

# .gitconfig
if [ ! -e $HOME/.gitconfig ] ; then
    echo -n "Use .gitconfig? (y/n) "
		read $answer
    case $answer in
        y)
            setup_git
            ;;
        Y)
            setup_git
            ;;
        yes)
            setup_git
            ;;
        *)
            ;;
    esac
fi

# ~/bin
echo -n "Copy shell scripts **for console** into ~/bin? (y/n) "
read answer
case $answer in
    y)
        setup_bin
        ;;
    Y)
        setup_bin
        ;;
    yes)
        setup_bin
        ;;
    *)
        echo
        ;;
esac

echo -n "Copy shell scripts **for X Desktop Environment** into ~/bin? (y/n) "
read answer
case $answer in
    y)
        setup_binx
        ;;
    Y)
        setup_binx
        ;;
    yes)
        setup_binx
        ;;
    *)
        echo
        ;;
esac

echo -n "Execute some commands to initialize vim environment? (y/n) "
read answer
case $answer in
    y)
        setup_vim
        ;;
    Y)
        setup_vim
        ;;
    yes)
        setup_vim
        ;;
    *)
        echo
        ;;
esac

echo "Finished setup"

