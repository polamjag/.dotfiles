# .zshrc
# polamjag <indirectgeeks@gmail.com>

# enable emacs-like keybind
bindkey -e

# ==========
# completion
# ==========
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
unsetopt menu_complete       # 補完の際に、可能なリストを表示してビープを鳴らすのではなく、
# 最初にマッチしたものをいきなり挿入、はしない
setopt auto_list             # display with list of all completion with ^I
setopt auto_menu             # complete automatically with key press of completion-key
setopt auto_param_keys       # complete parens automatically
setopt auto_resume           # resume suspended command automatically
compdef mosh=ssh             # override mosh completion with ssh

#zle -N self-insert self-insert-incr
#zle -N vi-cmd-mode-incr
#zle -N vi-backward-delete-char-incr
#zle -N backward-delete-char-incr
#zle -N expand-or-complete-prefix-incr
#compinit

#bindkey -M emacs '^h' backward-delete-char-incr
#bindkey -M emacs '^i' expand-or-complete-prefix-incr

#unsetopt automenu
#compdef -d scp
#compdef -d tar
#compdef -d make
#compdef -d java
#compdef -d svn
#compdef -d cvs
#
#now_predict=0
#
#function limit-completion
#{
#    if ((compstate[nmatches] <= 1)); then
#	zle -M ""
#    elif ((compstate[list_lines] > 6)); then
#	compstate[list]=""
#	zle -M "too many matches"
#	fi
#}
#
#function correct-prediction
#{
#    if ((now_predict == 1)); then
#	if [[ "$BUFFER" != "$buffer_prd" ]] || ((CURSOR != cursor_org)); then
#	    now_predict=0
#	fi
#    fi
#}
#
#function remove-prediction
#{
#    if ((now_predict == 1)); then
#	BUFFER="$buffer_org"
#	now_predict=0
#    fi
#}
#
#function show-prediction
#{
#    # assert(now_predict == 0)
#    if
#	((PENDING == 0)) &&
#	((CURSOR > 1)) &&
#	[[ "$PREBUFFER" == "" ]] &&
#	[[ "$BUFFER[CURSOR]" != " " ]]
#    then
#	cursor_org="$CURSOR"
#	buffer_org="$BUFFER"
#	comppostfuncs=(limit-completion)
#	zle complete-word
#	cursor_prd="$CURSOR"
#	buffer_prd="$BUFFER"
#	if [[ "$buffer_org[1,cursor_org]" == "$buffer_prd[1,cursor_org]" ]]; then
#	    CURSOR="$cursor_org"
#	    if [[ "$buffer_org" != "$buffer_prd" ]] || ((cursor_org != cursor_prd)); then
#		now_predict=1
#	    fi
#	else
#	    BUFFER="$buffer_org"
#	    CURSOR="$cursor_org"
#	fi
#	echo -e -n "\e[32m"
#    else
#	zle -M ""
#    fi
#}
#
#function preexec
#{
#    echo -e -n "\e[39m"
#}
#
#function vi-cmd-mode-incr
#{
#    correct-prediction
#    remove-prediction
#    zle vi-cmd-mode
#}
#
#function self-insert-incr
#{
#    correct-prediction
#    remove-prediction
#    if zle .self-insert; then
#	show-prediction
#    fi
#}

#function vi-backward-delete-char-incr
#{
#    correct-prediction
#    remove-prediction
#    if zle vi-backward-delete-char; then
#	show-prediction
#    fi
#}
#
#function backward-delete-char-incr
#{
#    correct-prediction
#    remove-prediction
#    if zle backward-delete-char; then
#	show-prediction
#    fi
#}
#
#function expand-or-complete-prefix-incr
#{
#    correct-prediction
#    if ((now_predict == 1)); then
#	CURSOR="$cursor_prd"
#	now_predict=0
#	comppostfuncs=(limit-completion)
#	zle list-choices
#    else
#	remove-prediction
#	zle expand-or-complete-prefix
#    fi
#}


##========================================================##
##====================== 履歴の設定 ======================##
##========================================================##
HISTFILE=$HOME/.zsh_history  # 履歴をファイルに保存する
HISTSIZE=100000              # メモリ内の履歴の数
SAVEHIST=100000              # 保存される履歴の数
setopt extended_history      # 履歴ファイルに開始時刻と経過時間を記録
setopt append_history        # 履歴を追加 (毎回 .zhistory を作るのではなく)
setopt inc_append_history    # 履歴をインクリメンタルに追加
setopt share_history         # 履歴の共有
#setopt hist_ignore_all_dups  # 重複するコマンド行は古い方を削除
setopt hist_ignore_dups      # 直前と同じコマンドラインはヒストリに追加しない
unsetopt hist_verify         # ヒストリを呼び出してから実行する間に一旦編集可能を止める
setopt hist_reduce_blanks    # 余分な空白は詰めて記録
#setopt hist_save_no_dups     # ヒストリファイルに書き出すときに、古いコマンドと同じものは無視する。
setopt hist_no_store         # historyコマンドは履歴に登録しない
setopt hist_expand           # 補完時にヒストリを自動的に展開

# 全履歴の一覧を出力する
function history-all { history -E 1 }

##========================================================##
##=================== プロンプトの設定 ===================##
##========================================================##
autoload -U promptinit ; promptinit
autoload -U colors     ; colors
autoload -U add-zsh-hook

# main prompt
PROMPT="
%F{yellow}%B%~%b%f (%F{cyan}%B%n%f@%F{cyan}%M%b%f): 
%B%#%b "
# config for right prompt which shows VCSs
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

#PROMPT="%{$reset_color%}%{$fg[green]%}$USER%{$reset_color%}@%{$fg[cyan]%}%m%{$reset_color%}$PROMPT"
#RPROMPT="%{$fg[green]%}[%*]%{$reset_color%}"
#RPROMPT="[%F{green}%B%.%b%f@%F{yellow}%U%m%u%f]"


## settings for moving directories
setopt auto_cd               # ディレクトリのみで移動
setopt auto_pushd            # 普通に cd するときにもディレクトリスタックにそのディレクトリを入れる
setopt pushd_ignore_dups     # ディレクトリスタックに重複する物は古い方を削除
setopt pushd_to_home         # pushd 引数ナシ == pushd $HOME
setopt pushd_silent          # pushd,popdの度にディレクトリスタックの中身を表示しない
# pop command
alias pd='popd'
alias gd='dirs -v; echo -n "select number: ";
read newdir; cd +"$newdir" '

## misc settings
setopt no_beep
setopt complete_in_word
setopt extended_glob
setopt brace_ccl             # ブレース展開機能を有効にする
setopt equals                # =COMMAND を COMMAND のパス名に展開
setopt numeric_glob_sort
setopt path_dirs             # コマンド名に / が含まれているとき PATH 中のサブディレクトリを探す
setopt print_eight_bit       # 補完候補リストの日本語を適正表示
setopt auto_name_dirs
unsetopt flow_control        # (shell editor 内で) C-s, C-q を無効にする
setopt no_flow_control       # C-s/C-q によるフロー制御を使わない
setopt hash_cmds             # 各コマンドが実行されるときにパスをハッシュに入れる
setopt bsd_echo
setopt no_hup                # ログアウト時にバックグラウンドジョブをkillしない
#setopt no_checkjobs          # ログアウト時にバックグラウンドジョブを確認しない
setopt notify                # バックグラウンドジョブが終了したら(プロンプトの表示を待たずに)すぐに知らせる
setopt long_list_jobs        # 内部コマンド jobs の出力をデフォルトで jobs -L にする
setopt magic_equal_subst     # コマンドラインの引数で --PREFIX=/USR などの = 以降でも補完できる
setopt multios               # 複数のリダイレクトやパイプなど、必要に応じて TEE や CAT の機能が使われる
setopt short_loops           # FOR, REPEAT, SELECT, IF, FUNCTION などで簡略文法が使えるようになる
setopt always_last_prompt    # カーソル位置は保持したままファイル名一覧を順次その場で表示
setopt cdable_vars sh_word_split
setopt rm_star_wait          # rm * を実行する前に確認
unsetopt no_clobber
setopt no_unset              # don't allow using of undefined variables

# configure behaviour of less (ref. man less)
LESS=-M
export LESS
if type /usr/bin/lesspipe &>/dev/null
then
LESSOPEN="| /usr/bin/lesspipe '%s'"
LESSCLOSE="/usr/bin/lesspipe '%s' '%s'"
export LESSOPEN LESSCLOSE
fi

umask 022 # ファイルを作るとき、どんな属性で作るか（man umask 参照）
ulimit -s unlimited  # stack size 制限解除
limit coredumpsize 0 # core 抑制
# Grip などGlibアプリケーション出力での文字化け防止
export G_FILENAME_ENCODING=@locale

# change title bar dynamically
precmd_2() {
[[ -t 1 ]] || return
case $TERM in
sun-cmd) print -Pn "\e]l[%~]\e\\"
 ;;
*xterm*|rxvt|(dt|k|E)term) print -Pn "\e]2;[%~]\a"
 ;;
esac
}
add-zsh-hook precmd precmd_2

# coloring for ls
eval $(dircolors -b ~/.dir_colors)

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
alias h='history'
alias ha='history-all'
alias ls='ls --color'
alias l='ls -CFa'
alias ll='ls -lh -F'
alias lla='ls -lh -a -F'
alias pse='ps aux | grep'
alias tm='tmux'
alias goog='w3m https://www.google.co.jp/'

# configs for well-used keys
bindkey "^[[3~" delete-char
bindkey "^[[1~" beginning-of-line
bindkey "^[[4~" end-of-line

# enable .zshenv for various environmental vars.
source ~/.zshenv
