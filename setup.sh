#!/bin/sh

# flags
FORCE_MODE=1

shdir="$(cd $(dirname $0) && pwd)" # where this script exists
cd ${shdir}

ask_exec() {
  echo -n "$1 [Y/n]: "
  shift
  read answer
  if [ "$answer" != "n" -a "$answer" != "N" ] ; then
    "$@"
  fi
}

make_symlink() {
  target="${2}"$(basename "$1")
  if [ -e "${target}" ] ; then
    if [ $FORCE_MODE = '0' ] ; then
      rm "$target"
      ln -s "$1" "$2"
    else
      ask_exec "file ${target} already exists. rename it and create symlink of new one[Y/n]?: " mv "$target" "${target}.old"
      ln -s "$1" "$2"
    fi
  else
    ln -s "$1" "$2"
  fi
}

setup_dot() {
  echo "[01;93m==> Setting up dotfiles ...[0m"
  for filepath in ${shdir}/.* ; do
    if [ \( -f $filepath -o -d $filepath \) -a \
      $filepath != "${shdir}/." -a \
      $filepath != "${shdir}/.." -a \
      $filepath != "${shdir}/.git" -a \
      $filepath != "${shdir}/.gitconfig" -a \
      $filepath != "${shdir}/.gitconfig.local" -a \
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
  make_symlink "${shdir}/.gitconfig" "$HOME"
  if [ ! -f "$HOME/.gitconfig.local" ] ; then
    echo -n "Input name[polamjag]> "
    read git_name
    echo -n "Input mail addr[s@polamjag.info]> "
    read git_mail
    if [ "$git_name" != "" -a "$git_mail" != "" ] ; then
      cat >>$HOME/.gitconfig.local <<EOF
[user]
user = ${git_name}
email = ${git_mail}
EOF
    else
      cp ${shdir}/.gitconfig.local ~/
    fi
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
  vim -u $HOME/.vimrc.ext -c 'NeoBundleInstall|q'
}
setup_godepends () {
  echo "[01;93m==> Setting up some go executables ...[0m"
  # repo list here
  go get github.com/peco/peco/cmd/peco
  go get github.com/motemen/ghq
}


# entrypoint
if [ "$1" = "--usage" ] ; then
  echo "$0 [--usage]"
  echo "$0 [-f|--force] [<args>]"
  echo "args: dot, git, bin, binx, vim, godepends"
  echo
  echo "e.g.: \`$0 dot git\`"
  exit 0
elif [ "$1" = '--force' -o "$1" = '-f' ] ; then
  FORCE_MODE=0
  shift
fi

if [ $# -gt 0 ] ; then
  echo -e "\x1B[01;95m-> Running in batch mode\x1B[0m"
  while [ $# -gt 0 ] ; do
    setup_$1
    shift
  done
  exit 0
else
  echo -e "\x1B[01;95m-> Running in interactive mode\x1B[0m"

  ask_exec "[01;92m> Setup dotfiles?[0m" setup_dot

  if [ ! -e $HOME/.gitconfig ] ; then
    ask_exec "[01;92m> Use .gitconfig?[0m" setup_git
  fi

  ask_exec "[01;92m> Copy shell scripts **for console** into ~/bin?[0m" setup_bin
  ask_exec "[01;92m> Copy shell scripts **for X Desktop Environment** into ~/bin?[0m" setup_binx
  ask_exec "[01;92m> Execute some commands to initialize vim environment?[0m" setup_vim
  ask_exec "[01;92m> Install some go executables?[0m" setup_godepends
fi

echo "-> Finished setup"

