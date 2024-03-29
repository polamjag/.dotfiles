alias sudo='sudo '
alias e='emacs -nw'
if hash vim &>/dev/null ; then
  alias vi='vim' ; fi
alias v='vi'
alias va='v -u $HOME/.vimrc.ext'
alias sl='ls'
alias h='history'
alias ha='history-all'
alias hag='history-all | grep'
alias pse='ps aux | grep'
alias ipa='ip a'
alias p8='ping 8.8.8.8'
alias pgo='ping google.com'
alias usdo='sudo'
alias le='less'
alias dign='dig +norec'
alias ag="ag --color-path='1;94'"

alias ntree="find . -print | sed '1d;s,[^/]*/,| ,g;s/..//;s/[^ ]*$/|--- &/'"

alias a='cd ../ ;'
alias b='cd - ;'
alias md='mkdir'

alias s='grec 2>/dev/null'
alias ser='grec'
alias f='find . | grep'
alias rbkup='rsync --progress -avr'
alias dirsize='du -h . | tail -n 1'
alias zzz="exec zsh"
alias tmesg="watch \"dmesg | tail -n 100 | tac\""
alias zzmv='noglob zmv -W'

alias dstat-full="dstat -tclmdrn"
alias dstat-mem="dstat -tclm"
alias dstat-cpu="dstat -tclr"
alias dstat-net="dstat -tclnd"
alias dstat-disk="dstat -tcldr"

alias delete-dead-symlink="find -L . -name . -o -type d -prune -o -type l -exec rm {} +"
alias watch="watch -c"
alias paste-ghq-get="xclip -o | xargs -I{} ghq get {}"
alias g="git"
alias gp='peco-ghq'
alias o='open'
alias bd='bundle'
man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
    man "$@"
}

tm() {
  if [ $# -gt 0 ] ; then
    tmux $@
  else
    if [ $(tmux list-sessions | wc -l) -gt 0 ] ; then
      tmux attach
    else
      tmux
    fi
  fi
}
alias tmn='tmux confirm-before "new-s"'

alias json2yaml="ruby -rjson -ryaml -e 'puts YAML.dump(JSON.parse(STDIN.read))'"
alias yaml2json="ruby -rjson -ryaml -e 'puts JSON.pretty_generate(YAML.load(STDIN.read))'"

function heisei() { echo $(( $1 - 1988 )) }

alias ncdu='ncdu --graph-style hash --color off'
