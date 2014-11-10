# polamjag/.dotfiles

## usage

clone this repo into your machine and run `/setup.sh` to make symbolic link of dotfiles into your `$HOME`.

## dependencies

### some commands written in go

install golang, set `$GOPATH` and run `setup.sh --force godepends`

### some vim plugins (w/ NeoBundle)

execute `./setup.sh --force vim` and run `:NeoBundleInstall` in vim (with alias `va` in .zsh.d/alias/general) to install all plugins.

## policy

- dont use 256 colors and non-ASCII chars in zsh and vim
  - keep vimrc simple but make .vimrc.ext rich, e.g. more plugins
