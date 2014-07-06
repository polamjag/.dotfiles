# polamjag/configs

## usage
clone this repo into your machine and run `/setup.sh` to make symbolic link of dotfiles into your `$HOME`.

## dependencies
### peco
set `$GOPATH` and run `go get github.com/lestrrat/peco/cmd/peco/` to setup peco.

### some vim plugins (w/ NeoBundle)
run as below to get ready of vimproc:
```
% git submodule init && git submodule update # in this repo
% cd .vim/bundle/vimproc/
% make
```
and run `:NeoBundleInstall` in vim (with alias `va` in .zsh.d/alias/general) to install all plugins.

