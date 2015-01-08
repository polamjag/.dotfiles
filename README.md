# polamjag/.dotfiles

## usage

clone this repo into your machine and run `/setup.sh` to make symbolic link of dotfiles into your `$HOME`, like:

```
cd && git clone https://github.com/polamjag/.dotfiles && .dotfiles/setup.sh
```

or you may save disk space with shallow clone:

```
cd && git clone --depth 1 https://github.com/polamjag/.dotfiles && .dotfiles/setup.sh
```

## dependencies

### some commands written in go

install golang, set `$GOPATH` and run `setup.sh godepends`

### some vim plugins (w/ NeoBundle)

execute `./setup.sh vim` to install all plugins.

## policy

- dont use 256 colors and non-ASCII chars in zsh and vim
  - keep .vimrc simple but make .vimrc.ext super rich
