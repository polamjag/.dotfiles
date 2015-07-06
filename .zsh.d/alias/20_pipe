alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g S='| sed'
alias -g A='| awk'
alias -g W='| wc'
if [ ! `hash peco >/dev/null 2>&1` ] ; then
 alias -g P='| peco' ; fi

alias -g UNCOLOR="| perl -pe 's/\e\[?.*?[\@-~]//g'"

if hash pbcopy >/dev/null 2>&1 ; then
  # OS X
  alias -g CP='| pbcopy'
elif hash xsel >/dev/null 2>&1 ; then
  # Linux
  alias -g CP='| xsel --input --clipboard'
elif hash putclip >/dev/null 2>&1 ; then
  # Cygwin
  alias -g CP='| putclip'
fi

