# polamjag/.dotfiles

## usage

clone this repo into your machine and run `/setup.sh` to make symbolic link of dotfiles into your `$HOME`, in one-liner:

```
curl https://raw.githubusercontent.com/polamjag/.dotfiles/master/setup.sh | sh
```

or manually:

```
cd && git clone https://github.com/polamjag/.dotfiles && .dotfiles/setup.sh
```

## dependencies

### some commands written in go and ruby

install golang, set `$GOPATH` (default is `$HOME`) and execute `setup.sh lib`

### some vim plugins (w/ NeoBundle)

execute `./setup.sh vim` to install all plugins.

