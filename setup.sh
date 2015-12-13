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
  echo "  Creating link: $1 -> $2"
  target="$(dirname $2)$(basename $1)"
  if [ -e "${target}" ] ; then
    if [ $FORCE_MODE = '0' ] ; then
      ln -sf "$1" "$2"
    else
      ask_exec "file ${target} already exists. rename it and create symlink of new one[Y/n]?: " mv "$target" "${target}.old"
      ln -s "$1" "$2"
    fi
  else
    ln -s "$1" "$2"
  fi
}
log_section() {
  echo -e "[01;93m==> $@[0m"
}
log_subsection() {
  echo -e "[01;95m-> $@[0m"
}

# main
setup_npmrc() {
  echo "prefix=$HOME/.npm" > $HOME/.npmrc
}
setup_dot() {
  log_section "Setting up dotfiles ..."
  skeldir="${shdir}/skel"
  for filepath in ${skeldir}/* ; do
    filename="$(basename $filepath)"
    if [ \( -f $filepath -o -d $filepath \) -a \
      $filepath != "${shdir}/gitconfig.local" -a \
      $filepath != "${shdir}/zshenv.exam" \
    ] ; then
      make_symlink "${filepath}" "${HOME}/.${filename}"
    fi
  done
  if [ ! -f $HOME/.zshenv ] ; then
    cp $shdir/.zshenv.exam $HOME/.zshenv
  fi
  if [ ! -f $HOME/.npmrc ] ; then
    setup_npmrc
    echo "  Wrote ~/.npmrc"
  fi
}
setup_git() {
  log_section "Setting up .gitconfig ..."
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
      cp ${shdir}/skel/gitconfig.local ~/.gitconfig.local
    fi
  fi
}
setup_bin () {
  log_section "Setting up ~/bin ..."
  if [ ! -d ${HOME}/bin ] ; then
    mkdir ${HOME}/bin
  fi
  for filepath in ${shdir}/bin/* ; do
    make_symlink "${filepath}" "${HOME}/bin/"
  done
}
setup_binx () {
  log_section "Setting up .dotfiles/bin_x to ~/bin ..."
  if [ ! -d ${HOME}/bin ] ; then
    mkdir ${HOME}/bin
  fi
  for filepath in ${shdir}/bin_x/* ; do
    make_symlink "${filepath}" "${HOME}/bin/"
  done
}
setup_emacs() {
  log_section "Setting up ~/.emacs.d/ ..."
  emacs --batch -q -l ${shdir}/skel/emacs.d/lisp/packages-list.el
}
setup_vim () {
  log_section "Setting up ~/.vim/ ..."
  cd $shdir
  git submodule init
  git submodule update
  cd $shdir/skel/vim/bundle/vimproc
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
  $HOME/.dotfiles/setup.sh --force dot git bin binx
}
setup_initial() {
  log_subsection "Running in interactive mode"

  ask_exec "-> Setup dotfiles?" setup_dot

  if [ ! -e $HOME/.gitconfig ] ; then
    ask_exec "-> Use .gitconfig?" setup_git
  fi

  ask_exec "-> Link common scripts into ~/bin?" setup_bin
  ask_exec "-> Link scripts for desktop into ~/bin?" setup_binx
  ask_exec "-> Initialize vim environment?" setup_vim
  ask_exec "-> Install some dependencies?" setup_lib
}

update_all() {
  if hash go >/dev/null 2>&1 ; then
    go get -u github.com/peco/peco/cmd/peco
    go get -u github.com/motemen/ghq
  fi
  vim -u $HOME/.vimrc.ext -c 'NeoBundleUpdate|q'
  cd $shdir/lib
  PATH="$PATH:$(gem env gempath | tr ':' '\n' | sed -e 's|$|/bin:|g' | tr -d '\n' | sed -e 's|:$||')" sh -c "bundle update"
  hash npm &>/dev/null && npm upgrade -g
}

# entrypoint
if [ $# -eq 0 ] ; then
  if [ $(basename "$0") != "setup.sh" ] ; then
    setup_initial_dl
  else
    setup_initial
  fi
else
while [ $# -gt 0 ] ; do
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
