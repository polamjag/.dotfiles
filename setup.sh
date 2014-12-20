#!/bin/sh

shdir="$(cd $(dirname $0) && pwd)" # where this script exists
cd ${shdir}

make_symlink() {
  target="${2}"$(basename "$1")
  if [ -e "${target}" ] ; then
    echo -n "file ${target} already exists. rename it and create symlink of new one[Y/n]?: "
    read ans
    if [ "$ans" != "n\n" ] ; then
      mv "$target" "${target}.old"
    fi
  fi
  ln -s "$1" "$2"
}

setup_dot() {
  echo "[01;93m==> Setting up dotfiles ...[0m"
  for filepath in ${shdir}/.* ; do
    if [ \( -f $filepath -o -d $filepath \) -a \
      $filepath != "${shdir}/." -a \
      $filepath != "${shdir}/.." -a \
      $filepath != "${shdir}/.git" -a \
      $filepath != "${shdir}/.gitconfig" -a \
      $filepath != "${shdir}/.gitignore" -a \
      $filepath != "${shdir}/.gitmodules" -a \
      $filepath != "${shdir}/.zshenv.exam" \
    ] ; then
      echo "  Creating link: ${filepath} -> ${HOME}"
      make_symlink "${filepath}" "${HOME}/"
    fi
  done
  if [ ! -f $HOME/.zshenv ] ; then
    cp $shdir/.zshenv.exam $HOME/.zshenv
  fi
}
setup_git() {
  echo "[01;93m==> Setting up .gitconfig ...[0m"
  ln -s ${shdir}/.gitconfig $HOME
  echo -n "Input name[polamjag]> "
  read git_name
  echo -n "Input mail addr[s@polamjag.info]> "
  read git_mail
  if [ "$git_name" = "\n" -a "$git_mail" = "\n" ] ; then
    cat >>$HOME/.gitconfig.local <<EOF
[user]
 user = ${git_name}
 email = ${git_mail}
EOF
  else
    cp ${shdir}/.gitconfig.local ~/
  fi
}
setup_bin () {
  echo "[01;93m==> Setting up ~/bin ...[0m"
  if [ ! -d ${HOME}/bin ] ; then
    mkdir ${HOME}/bin
  fi
  for filepath in ${shdir}/bin/* ; do
    echo "  Creating link: ${filepath} -> ${HOME}/bin/"
    make_symlink "${filepath}" "${HOME}/bin/"
  done
}
setup_binx () {
  echo "[01;93m==> Setting up ~/bin_x ...[0m"
  if [ ! -d ${HOME}/bin ] ; then
    mkdir ${HOME}/bin
  fi
  for filepath in ${shdir}/bin_x/* ; do
    echo "  Creating link: ${filepath} -> ${HOME}/bin/"
    make_symlink "${filepath}" "${HOME}/bin/"
  done
}
setup_vim () {
  echo "[01;93m==> Setting up ~/.vim/ ...[0m"
  cd $shdir
  git submodule init
  git submodule update
  cd $shdir/.vim/bundle/vimproc
  make
  zsh -ic "va -c 'NeoBundleInstall|q'"
}
setup_godepends () {
  echo "[01;93m==> Setting up some go executables ...[0m"
  # repo list here
  go get github.com/peco/peco/cmd/peco
  go get github.com/motemen/ghq
}

if [ $# -eq 1 -a "$1" = "--usage" ] ; then
  echo "$0 [--usage] [<args>]"
  echo "args: dot, git, bin, binx, vim, godepends"
  echo
  echo "e.g.: \`$0 dot git\`"
  exit 0
elif [ $# -gt 0 ] ; then
  echo -e "\x1B[01;95m-> Running in batch mode\x1B[0m"
  while [ $# -gt 0 ] ; do
    setup_$1
    shift
  done
  exit 0
fi

# interactive mode
echo -e "\x1B[01;95m-> Running in interactive mode\x1B[0m"

# common files
echo -n "[01;92m> Setup dotfiles? (y/n) [0m"
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
  echo -n "[01;92m> Use .gitconfig? (y/n) [0m"
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
echo -n "[01;92m> Copy shell scripts **for console** into ~/bin? (y/n) [0m"
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

echo -n "[01;92m> Copy shell scripts **for X Desktop Environment** into ~/bin? (y/n) [0m"
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

echo -n "[01;92m> Execute some commands to initialize vim environment? (y/n) [0m"
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

echo -n "[01;92m> Install some go executables? (y/n) [0m"
read answer
case $answer in
  y)
    setup_godepends
    ;;
  Y)
    setup_godepends
    ;;
  yes)
    setup_godepends
    ;;
  *)
    echo
    ;;
esac

echo "-> Finished setup"

