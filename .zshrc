# .zshrc - polamjag <s@polamjag.info>

if [ -e ~/.zshenv ] ; then
		source ~/.zshenv
fi

# enable emacs-like keybind
bindkey -e
# tab width in shell
tabs -2

autoload colors ; colors


# ========== #
# completion #
# ========== #
autoload -U compinit ; compinit
setopt list_packed           # display completion compactly
setopt listpacked
unsetopt auto_remove_slash
setopt auto_param_slash      # append '/' at tail of directory
setopt mark_dirs             
setopt list_types            # display type of files in completion list
unsetopt menu_complete
setopt auto_list             # display with list of all completion with ^I
setopt auto_menu             # complete automatically with key press
setopt auto_param_keys       # complete parens automatically
setopt auto_resume           # resume suspended command automatically

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # case-insensitive
zstyle ':completion:*' use-cache true        # cache completion
zstyle ':completion:*' verbose yes
zstyle ':completion:*' completer _expand _complete _match _prefix _approximate _list _history
zstyle ':completion:*:default' menu select=1 # select completion with arrow keys
zstyle ':completion:*:messages' format '%F{green}%d'
zstyle ':completion:*:warnings' format '%F{magenta}No matches for:''%F{YELLOW} %d'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:descriptions' format '%F{yellow}%B%d: %b%f'
zstyle ':completion:*:processes' command 'ps x'
zstyle ':completion:*' group-name ''
# enable coloring for completion of kill command
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([%0-9]#)*=0=01;31'
# completion overrides
compdef mosh=ssh


# ======= #
# history #
# ======= #
HISTFILE=$HOME/.zsh_history 
HISTSIZE=100000
SAVEHIST=100000
setopt extended_history
setopt append_history
setopt inc_append_history
setopt share_history
setopt hist_ignore_dups  # ignore same command as above
unsetopt hist_verify
setopt hist_reduce_blanks
setopt hist_no_store 
setopt hist_expand  # expand history in completion
# output all histories 
function history-all { history -E 1 }
# enable completion from history
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end


# ====== #
# prompt #
# ====== #
autoload -U promptinit ; promptinit
autoload -U add-zsh-hook
setopt correct
ip_addr_disp () {
  if [ -e `which ip` ] ; then
    echo -n '@'
    ip addr | grep inet | grep -v 127.0.0.1 | grep -v \:\:1 | grep -oE \(\[0-9\]\{1,3\}\[.\]\[0-9\]\{1,3\}\[.\]\[0-9\]\{1,3\}\[.\]\[0-9\]\{1,3\}\)/ | grep -oE \(\[0-9\]\{1,3\}\[.\]\[0-9\]\{1,3\}\[.\]\[0-9\]\{1,3\}\[.\]\[0-9\]\{1,3\}\)
  fi
}
ssh_prefix () {
  if [ "${SSH_CONNECTION:+mayuge}" = mayuge ] ; then
    echo -n "%F{magenta}%B-=> %b%f"
  else
    echo -n ""
  fi
}
# main prompt
PROMPT="
`ssh_prefix`%F{green}%B%~%b%f (%B%F{yellow}%M%f::%F{cyan}%n%f%b): 
%(?.%F{default}.%F{red}!)%B%#%b%f "
# config for right prompt which shows VCS
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '@%s:%b'
zstyle ':vcs_info:*' actionformats '@%s:%b|%a'
precmd_1 () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
} ; add-zsh-hook precmd precmd_1
RPROMPT="%F{blue}%B%1(v|%1v|)%b%f"
SPROMPT='%BCorrect: %F{yellow}%R%f -> %F{cyan}%r%f [nyae]?%b '


# =============================== #
# settings for moving directories #
# =============================== #
setopt auto_cd 
setopt auto_pushd 
setopt pushd_ignore_dups 
setopt pushd_to_home  
setopt pushd_silent  
alias pd='popd'
alias gd='dirs -v; echo -n "select number: ";
read newdir; cd +"$newdir" '


# ==== #
# misc #
# ==== #
setopt no_beep
setopt nolistbeep
setopt complete_in_word
setopt extended_glob
setopt brace_ccl
setopt equals
setopt numeric_glob_sort
setopt path_dirs
setopt print_eight_bit
setopt auto_name_dirs
unsetopt flow_control
setopt no_flow_control
setopt hash_cmds
setopt bsd_echo
setopt no_hup
setopt notify
setopt long_list_jobs
setopt magic_equal_subst
setopt multios
setopt short_loops
setopt always_last_prompt
setopt cdable_vars sh_word_split
setopt rm_star_wait
unsetopt no_clobber
setopt no_unset # don't allow using of undefined variables
setopt interactive_comments
# configure behaviour of less (ref. man less)
LESS=-M
export LESS
if type /usr/bin/lesspipe &>/dev/null ; then
		LESSOPEN="| /usr/bin/lesspipe '%s'"
		LESSCLOSE="/usr/bin/lesspipe '%s' '%s'"
		export LESSOPEN LESSCLOSE
fi
umask 022
ulimit -s unlimited
# fix corruption in Glib application
export G_FILENAME_ENCODING=@locale
export WORDCHARS="*?_-.[]~&;!#$%^(){}<>"


# ================== #
# command line stack #
# ================== #
show_buffer_stack() {
    POSTDISPLAY="
    stack: $LBUFFER"
    zle push-line
}
zle -N show_buffer_stack
bindkey "^[q" show_buffer_stack


# ===================== #
# aliases and key binds #
# ===================== #
# alias for fix of less with colored output
alias less='less --raw -R'
# global aliases with pipe
alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g S='| sed'
alias -g A='| awk'
alias -g W='| wc'
# general aliases
alias e='emacs -nw'
if [ -e `which vim` ] ; then
  alias vi='vim' ; fi
alias history='history -f'
alias h='history'
alias ha='history-all'
alias pse='ps aux | grep'
alias le='less'
alias free='free -m'
alias a='cd ../ ;'
alias md='mkdir'
alias goog='w3m https://www.google.co.jp/ -cookie'
alias gst='git branch -a ; echo ; git status'
alias gcm='git commit -m'
alias ga='git add'
# configs for well-used keys
bindkey "^[[3~" delete-char
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line


# =========================== #
# load external configulation #
# =========================== #
case ${OSTYPE} in
    darwin*)
        source $HOME/.zsh.d/bsd
        ;;
		freebsd*)
				source $HOME/.zsh.d/bsd
				;;
    linux*)
        source $HOME/.zsh.d/linux
        ;;
esac


zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} 
