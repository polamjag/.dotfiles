# ~/.bashrc

if [ -e ${HOME}/.zshenv ] ; then
    source ~/.zshenv ; fi
case ${OSTYPE} in
    freebsd*|darwin*)
        if [ -e ${HOME}/.zsh.d/bsd ] ; then
            source $HOME/.zsh.d/bsd
        fi
        ;;
    linux*)
        if [ -e ${HOME}/.zsh.d/linux ] ; then
            source $HOME/.zsh.d/linux
        fi
        ;;
esac

set -o emacs

PS1="\[\n\e[1;32m\W\e[1;37m b(\e[1;33m\H\e[1;37m::\e[1;36m\u\e[1;37m)\n$\e[0m \]"

bind '"\C-l": clear-screen ; hash -r'
