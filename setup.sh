#!/bin/bash
set -euo pipefail

FORCE_MODE=0
shdir="$(cd "$(dirname "$0")" && pwd)"

# helpers
_usage() {
  cat <<EOF
Usage:
  $0                        interactive setup
  $0 [-f|--force] <command>...

Commands:
  dot       link dotfiles (skel/* -> ~/.*) and create ~/.zshenv, ~/.npmrc if missing
  git       create ~/.gitconfig.local
  bin       link bin/* into ~/bin
  binx      link bin_x/* into ~/bin
  emacs     install emacs packages
  vim       install vim plugins
  lib       install go/ruby dependencies
  zed       link Zed config into ~/.config
  ghostty   link Ghostty config into ~/.config/ghostty
  update    update go tools, vim plugins, gems and npm packages

Options:
  -f, --force   overwrite existing files without asking

Example:
  $0 dot git bin
EOF
}

log_section() {
  printf '\033[01;93m==> %s\033[0m\n' "$*"
}

log_subsection() {
  printf '\033[01;95m-> %s\033[0m\n' "$*"
}

ask() {
  printf '%s [Y/n]: ' "$1"
  local answer
  read -r answer || answer="n"
  [ "$answer" != "n" ] && [ "$answer" != "N" ]
}

ask_exec() {
  local prompt="$1"
  shift
  if ask "$prompt"; then
    "$@"
  fi
}

# make_symlink <src> <dest>
# <dest> ending with "/" is treated as a directory to place the link in;
# otherwise <dest> is the link path itself.
make_symlink() {
  local src="$1" dest="$2" target

  if [ "${dest%/}" != "$dest" ]; then
    target="${dest%/}/$(basename "$src")"
  else
    target="$dest"
  fi

  if [ -L "$target" ] && [ "$(readlink "$target")" = "$src" ]; then
    echo "  Already linked: $target"
    return 0
  fi

  if [ -e "$target" ] || [ -L "$target" ]; then
    if [ "$FORCE_MODE" -eq 1 ]; then
      echo "  Overwriting: $target -> $src"
      ln -sfn "$src" "$target"
    elif ask "  $target already exists. Rename to $target.old and link?"; then
      mv "$target" "$target.old"
      echo "  Creating link: $src -> $target"
      ln -s "$src" "$target"
    else
      echo "  Skipped: $target"
    fi
  else
    echo "  Creating link: $src -> $target"
    ln -s "$src" "$target"
  fi
}

# run a command with gem bin directories appended to PATH
with_gem_path() {
  local gem_bin_path
  gem_bin_path="$(gem env gempath | tr ':' '\n' | sed -e 's|$|/bin:|g' | tr -d '\n' | sed -e 's|:$||')"
  PATH="${PATH}:${gem_bin_path}" "$@"
}

# main
setup_npmrc() {
  echo "prefix=$HOME/.npm" > "$HOME/.npmrc"
}

setup_dot() {
  log_section "Setting up dotfiles ..."
  local skeldir="${shdir}/skel"
  local filepath filename
  for filepath in "${skeldir}"/*; do
    filename="$(basename "$filepath")"
    case "$filename" in
      gitconfig.local | zshenv.exam | *.un~ | .DS_Store) continue ;;
    esac
    if [ -f "$filepath" ] || [ -d "$filepath" ]; then
      make_symlink "$filepath" "${HOME}/.${filename}"
    fi
  done
  if [ ! -f "$HOME/.zshenv" ]; then
    cp "${skeldir}/zshenv.exam" "$HOME/.zshenv"
    echo "  Copied ~/.zshenv from zshenv.exam"
  fi
  if [ ! -f "$HOME/.npmrc" ]; then
    setup_npmrc
    echo "  Wrote ~/.npmrc"
  fi
}

setup_git() {
  log_section "Setting up .gitconfig ..."
  if [ -e "$HOME/.gitconfig.local" ]; then
    echo "  ~/.gitconfig.local already exists"
    return 0
  fi
  local git_name git_mail
  echo -n "Input name[polamjag]> "
  read -r git_name || true
  echo -n "Input mail addr[s@polamjag.info]> "
  read -r git_mail || true
  if [ -n "$git_name" ] && [ -n "$git_mail" ]; then
    cat > "$HOME/.gitconfig.local" <<EOF
[user]
	name = ${git_name}
	email = ${git_mail}
EOF
    echo "  Wrote ~/.gitconfig.local"
  else
    cp "${shdir}/skel/gitconfig.local" "$HOME/.gitconfig.local"
    echo "  Copied skeleton ~/.gitconfig.local"
  fi
}

link_into_bin() {
  local srcdir="$1" filepath
  mkdir -p "$HOME/bin"
  for filepath in "${srcdir}"/*; do
    if [ -f "$filepath" ]; then
      make_symlink "$filepath" "$HOME/bin/"
    fi
  done
}

setup_bin() {
  log_section "Setting up ~/bin ..."
  link_into_bin "${shdir}/bin"
}

setup_binx() {
  log_section "Setting up .dotfiles/bin_x to ~/bin ..."
  link_into_bin "${shdir}/bin_x"
}

setup_emacs() {
  log_section "Setting up ~/.emacs.d/ ..."
  emacs --batch -q -l "${shdir}/skel/emacs.d/lisp/packages-list.el"
}

setup_vim() {
  log_section "Setting up ~/.vim/ ..."
  git -C "$shdir" submodule update --init
  make -C "${shdir}/skel/vim/bundle/vimproc.vim"
  vim -u "$HOME/.vimrc.ext" -c 'NeoBundleInstall|q'
}

setup_lib() {
  log_section "Setting up some libraries and commands ..."
  if hash go >/dev/null 2>&1; then
    go install github.com/peco/peco/cmd/peco@latest
    go install github.com/motemen/ghq@latest
  fi
  if hash gem >/dev/null 2>&1; then
    gem install bundler
    (cd "${shdir}/lib" && with_gem_path bundle)
  fi
}

setup_zed() {
  log_section "Setting up Zed config ..."
  mkdir -p "$HOME/.config"
  make_symlink "${shdir}/config/zed" "$HOME/.config/"
}

setup_ghostty() {
  log_section "Setting up Ghostty config ..."
  mkdir -p "$HOME/.config/ghostty"
  make_symlink "${shdir}/config/ghostty" "$HOME/.config/ghostty/config"
}

setup_initial_dl() {
  cd "$HOME"
  git clone https://github.com/polamjag/.dotfiles
  "$HOME/.dotfiles/setup.sh" --force dot git bin binx
}

setup_initial() {
  log_subsection "Running in interactive mode"

  ask_exec "-> Setup dotfiles?" setup_dot

  if [ ! -e "$HOME/.gitconfig" ]; then
    ask_exec "-> Use .gitconfig?" setup_git
  fi

  ask_exec "-> Link common scripts into ~/bin?" setup_bin
  ask_exec "-> Link scripts for desktop into ~/bin?" setup_binx
  ask_exec "-> Initialize vim environment?" setup_vim
  ask_exec "-> Install some dependencies?" setup_lib
}

update_all() {
  if hash go >/dev/null 2>&1; then
    go install github.com/peco/peco/cmd/peco@latest
    go install github.com/motemen/ghq@latest
  fi
  vim -u "$HOME/.vimrc.ext" -c 'NeoBundleUpdate|q'
  if hash gem >/dev/null 2>&1; then
    (cd "${shdir}/lib" && with_gem_path bundle update)
  fi
  if hash npm >/dev/null 2>&1; then
    npm upgrade -g
  fi
}

# entrypoint
if [ $# -eq 0 ]; then
  if [ "$(basename "$0")" != "setup.sh" ]; then
    setup_initial_dl
  else
    setup_initial
  fi
  exit 0
fi

while [ $# -gt 0 ]; do
  case "$1" in
    -h | --help | --usage)
      _usage
      exit 0
      ;;
    -f | --force)
      FORCE_MODE=1
      ;;
    update)
      update_all
      exit 0
      ;;
    dot | git | bin | binx | emacs | vim | lib | zed | ghostty)
      "setup_$1"
      ;;
    *)
      echo "Unknown command: $1" >&2
      _usage
      exit 1
      ;;
  esac
  shift
done
