# polamjag/configs

## usage
clone this repo into your machine and run `/setup.sh` to make symbolic link of dotfiles into your `$HOME`.

## dependencies
### peco
set `$GOPATH` and run `go get github.com/lestrrat/peco/cmd/peco/` to setup peco.

### some vim plugins (w/ NeoBundle)
execute `./setup.sh vim` and run `:NeoBundleInstall` in vim (with alias `va` in .zsh.d/alias/general) to install all plugins.

