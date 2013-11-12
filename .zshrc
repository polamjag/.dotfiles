# .zshrc
# polamjag <indirectgeeks@gmail.com>

if [ -e ~/.zshenv ] ; then
	source ~/.zshenv
fi

# enable emacs-like keybind
bindkey -e
# tab width in shell
tabs -2


# ========== #
# completion #
# ========== #
autoload -U compinit ; compinit
# enable coloring for completion
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
# enable coloring for completion of kill command
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([%0-9]#)*=0=01;31'
# ignore case for completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*:default' menu select=1 # select completion with arrow keys
zstyle ':completion:*' use-cache true        # cache completion
zstyle ':completion:*:processes' command 'ps x'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} # colored file completion

setopt list_packed           # display completion compactly
unsetopt auto_remove_slash
setopt auto_param_slash      # append '/' at tail of directory in completion automatically
setopt mark_dirs             
setopt list_types            # display type of files in completion list as ls -F
unsetopt menu_complete
setopt auto_list             # display with list of all completion with ^I
setopt auto_menu             # complete automatically with key press of completion-key
setopt auto_param_keys       # complete parens automatically
setopt auto_resume           # resume suspended command automatically
compdef mosh=ssh             # override mosh completion with ssh


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
setopt hist_ignore_dups      # ignore same command as above
unsetopt hist_verify
setopt hist_reduce_blanks
setopt hist_no_store 
setopt hist_expand  # expand history in completion
# output all histories 
function history-all { history -E 1 }


# ====== #
# prompt #
# ====== #
autoload -U promptinit ; promptinit
autoload -U colors     ; colors
autoload -U add-zsh-hook
# main prompt
PROMPT="
%F{green}%B%~%b%f (%B%F{yellow}%M%f::%F{cyan}%n%f%b): 
%(?.%F{white}.%F{red})%B%#%b%f "
#`(){ if [[ %? -ne 0 ]] ; then ; %F{yellow} ; else ; ; fi }`%B%#%b "
# config for right prompt which shows VCSs; Version Control Systems
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'

precmd_1 () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
add-zsh-hook precmd precmd_1
RPROMPT="%B%1(v|%F{magenta}%1v%f|)%b"


# =============================== #
# settings for moving directories #
# =============================== #
setopt auto_cd 
setopt auto_pushd 
setopt pushd_ignore_dups 
setopt pushd_to_home  
setopt pushd_silent  
# pop command
alias pd='popd'
alias gd='dirs -v; echo -n "select number: ";
read newdir; cd +"$newdir" '


# ==== #
# misc #
# ==== #
setopt no_beep
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

# configure behaviour of less (ref. man less)
LESS=-M
export LESS
if type /usr/bin/lesspipe &>/dev/null
then
LESSOPEN="| /usr/bin/lesspipe '%s'"
LESSCLOSE="/usr/bin/lesspipe '%s' '%s'"
export LESSOPEN LESSCLOSE
fi

umask 022
ulimit -s unlimited
limit coredumpsize 0
# fix corruption in Glib application
export G_FILENAME_ENCODING=@locale

# coloring for ls
if [ -e ~/.dir_colors ] ; then
	eval $(dircolors -b ~/.dir_colors)
fi

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

# alias
alias e='emacs -nw'
alias history='history -f'
alias h='history'
alias ha='history-all'
alias ls='ls --color'
alias l='ls -CFa'
alias ll='ls -lh -F'
alias lla='ls -lh -a -F'
alias pse='ps aux | grep'
alias tm='tmux'
alias free='free -m'
alias goog='w3m https://www.google.co.jp/ -cookie'
alias gst='git branch -a ; echo ; git status'

# configs for well-used keys
bindkey "^[[3~" delete-char
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line

