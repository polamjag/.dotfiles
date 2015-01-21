#!/bin/sh

# flags
FORCE_MODE=1
shdir="$(cd $(dirname $0) && pwd)" # where this script exists
cd ${shdir}

# helpers
_usage() {
  echo "$0 [--usage]"
  echo "$0 [-f|--force] update|[<args>]"
  echo "args: dot, git, bin, binx, vim, lib"
  echo
  echo "e.g.: \`$0 dot git\`"
}
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
log_section() {
  echo "[01;93m==> $@[0m"
}

# main
setup_dot() {
  log_section "Setting up dotfiles ..."
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
  log_section "Setting up .gitconfig ..."
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
  log_section "Setting up ~/bin ..."
  if [ ! -d ${HOME}/bin ] ; then
    mkdir ${HOME}/bin
  fi
  for filepath in ${shdir}/bin/* ; do
    echo "  Creating link: ${filepath} -> ${HOME}/bin/"
    make_symlink "${filepath}" "${HOME}/bin/"
  done
}
setup_binx () {
  log_section "Setting up ~/bin_x ..."
  if [ ! -d ${HOME}/bin ] ; then
    mkdir ${HOME}/bin
  fi
  for filepath in ${shdir}/bin_x/* ; do
    echo "  Creating link: ${filepath} -> ${HOME}/bin/"
    make_symlink "${filepath}" "${HOME}/bin/"
  done
}
setup_vim () {
  log_section "Setting up ~/.vim/ ..."
  cd $shdir
  git submodule init
  git submodule update
  cd $shdir/.vim/bundle/vimproc
  make
  vim -u $HOME/.vimrc.ext -c 'NeoBundleInstall|q'
}
setup_lib() {
  log_section "Setting up some libraries and commands ..."
  go get github.com/peco/peco/cmd/peco
  go get github.com/motemen/ghq
  cd $shdir/lib
  if hash gem &>/dev/null ; then
    gem install bundler
    PATH="$PATH:$(gem env gempath | tr ':' '\n' | sed -e 's|$|/bin:|g' | tr -d '\n' | sed -e 's|:$||')" sh -c bundle
  fi
}

setup_initial_dl() {
  cd
  git clone https://github.com/polamjag/.dotfiles
  $HOME/.dotfiles/setup.sh --force dot git bin binx vim
}
setup_initial() {
  echo -e "\x1B[01;95m-> Running in interactive mode\x1B[0m"

  ask_exec "[01;92m> Setup dotfiles?[0m" setup_dot

  if [ ! -e $HOME/.gitconfig ] ; then
    ask_exec "[01;92m> Use .gitconfig?[0m" setup_git
  fi

  ask_exec "[01;92m> Copy shell scripts **for console** into ~/bin?[0m" setup_bin
  ask_exec "[01;92m> Copy shell scripts **for X Desktop Environment** into ~/bin?[0m" setup_binx
  ask_exec "[01;92m> Execute some commands to initialize vim environment?[0m" setup_vim
  ask_exec "[01;92m> Install some go executables?[0m" setup_godepends
}

update_all() {
  go get -u github.com/peco/peco/cmd/peco
  go get -u github.com/motemen/ghq
  vim -u $HOME/.vimrc.ext -c 'NeoBundleUpdate|q'
  cd $shdir/lib
  PATH="$PATH:$(gem env gempath | tr ':' '\n' | sed -e 's|$|/bin:|g' | tr -d '\n' | sed -e 's|:$||')" sh -c "bundle update"
}

# entrypoint
if [ $# -eq 0 ] ; then
  if [ $(basename "$0") != "setup.sh" ] ; then
    setup_initial_dl
  else
    setup_initial
  fi
else
while [[ $# -gt 0 ]] ; do
  case "$1" in
    -h|--help|--usage)
      _usage
      exit 0
      ;;
    -f|--force)
      FORCE_MODE=0
      shift
      ;;
    update)
      update_all
      exit 0
      ;;
    *)
      setup_$1
      shift
      ;;
  esac
done
fi
