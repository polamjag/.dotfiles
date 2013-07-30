## <エスケープシーケンス>
## prompt_bang が有効な場合、!=現在の履歴イベント番号, !!='!' (リテラル)
# ${WINDOW:+"[$WINDOW]"} = screen 実行時にスクリーン番号を表示 (prompt_subst が必要)
# %/ or %d = ディレクトリ (0=全て, -1=前方からの数)
# %~ = ディレクトリ
# %h or %! = 現在の履歴イベント番号
# %L = 現在の $SHLVL の値
# %M = マシンのフルホスト名
# %m = ホスト名の最初の `.' までの部分
# %S (%s) = 突出モードの開始 (終了)
# %U (%u) = 下線モードの開始 (終了)
# %B (%b) = 太字モードの開始 (終了)
# %t or %@ = 12 時間制, am/pm 形式での現在時刻
# %n or $USERNAME = ユーザー ($USERNAME は環境変数なので setopt prompt_subst が必要)
# %N = シェル名
# %i = %N によって与えられるスクリプト, ソース, シェル関数で, 現在実行されている行の番号 (debug用)
# %T = 24 時間制での現在時刻
# %* = 24 時間制での現在時刻, 秒付き
# %w = `曜日-日' の形式での日付
# %W = `月/日/年' の形式での日付
# %D = `年-月-日' の形式での日付
# %D{string} = strftime 関数を用いて整形された文字列 (man 3 strftime でフォーマット指定が分かる)
# %l = ユーザがログインしている端末から, /dev/ プレフィックスを取り除いたもの
# %y = ユーザがログインしている端末から, /dev/ プレフィックスを取り除いたもの (/dev/tty* はソノママ)
# %? = プロンプトの直前に実行されたコマンドのリターンコード
# %_ = パーサの状態
# %E = 行末までクリア
# %# = 特権付きでシェルが実行されているならば `#', そうでないならば `%' == %(!.#.%%)
# %v = psvar 配列パラメータの最初の要素の値
# %{...%} = リテラルのエスケープシーケンスとして文字列をインクルード
# %(x.true-text.false-text) = 三つ組の式
# %<,>string>, %[xstring] = プロンプトの残りの部分に対する, 切り詰めの振る舞い
#         `<' の形式は文字列の左側を切り詰め, `>' の形式は文字列の右側を切り詰めます
# %c, %., %C = $PWD の後ろ側の構成要素

# .zshrc をコンパイルして .zshrc.zwc を生成するコマンド
zcompile .zshrc

# コマンドラインスタック（入力中コマンドをスタックに退避させる）
# ESC-q

# コマンド入力中にヘルプ（man）を見る
# ESC-h

##========================================================##
##================== キーバインドの設定 ==================##
##========================================================##
bindkey -e      # emacs キーバインド


##========================================================##
##================= リストの色つけの設定 =================##
##========================================================##
# ls, #dir, vdir の設定
#alias s='screen -U'

alias tm='tmux'

export MAILCHECK=0
# 補完候補にも色付き表示
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
# kill の候補にも色付き表示
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([%0-9]#)*=0=01;31'

##========================================================##
##====================== 補完の設定 ======================##
##========================================================##
autoload -U compinit ; compinit
# 補完候補の大文字小文字の違いを無視
#zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*:default' menu select=1 # 補完候補を←↓↑→で選択
zstyle ':completion:*' use-cache true        # 補完キャッシュ
# kill で 'ps x' のリストから選択可能
zstyle ':completion:*:processes' command 'ps x'

setopt list_packed           # コンパクトに補完リストを表示
#setopt auto_remove_slash     # 補完で末尾に補われた / を自動的に削除
unsetopt auto_remove_slash
setopt auto_param_slash      # ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt mark_dirs             # ファイル名の展開でディレクトリにマッチした場合 末尾に / を付加
setopt list_types            # 補完候補一覧でファイルの種別を識別マーク表示 (訳注:ls -F の記号)
unsetopt menu_complete       # 補完の際に、可能なリストを表示してビープを鳴らすのではなく、
                        # 最初にマッチしたものをいきなり挿入、はしない
setopt auto_list             # ^Iで補完可能な一覧を表示する(補完候補が複数ある時に、一覧表示)
setopt auto_menu             # 補完キー連打で順に補完候補を自動で補完
setopt auto_param_keys       # カッコの対応などを自動的に補完
setopt auto_resume           # サスペンド中のプロセスと同じコマンド名を実行した場合はリジューム

#setopt auto_correct          # 補完時にスペルチェック
#setopt correct               # スペルミスを補完
#setopt correct_all           # コマンドライン全てのスペルチェックをする

##========================================================##
##==================== 予測補完の設定 ====================##
##========================================================##
autoload -U predict-on       # 履歴による予測入力 (man zshcontrib)
zle -N predict-on
zle -N predict-off
bindkey '^xp'  predict-on    # Cttl+x p で予測オン
bindkey '^x^p' predict-off   # Cttl+x Ctrl+p で予測オフ

##========================================================##
##====================== 履歴の設定 ======================##
##========================================================##
HISTFILE=$HOME/.zsh_history  # 履歴をファイルに保存する
HISTSIZE=100000              # メモリ内の履歴の数
SAVEHIST=100000              # 保存される履歴の数
setopt extended_history      # 履歴ファイルに開始時刻と経過時間を記録
#unsetopt extended_history
setopt append_history        # 履歴を追加 (毎回 .zhistory を作るのではなく)
setopt inc_append_history    # 履歴をインクリメンタルに追加
setopt share_history         # 履歴の共有
setopt hist_ignore_all_dups  # 重複するコマンド行は古い方を削除
setopt hist_ignore_dups      # 直前と同じコマンドラインはヒストリに追加しない
setopt hist_ignore_space     # スペースで始まるコマンド行はヒストリリストから削除
                        # (→ 先頭にスペースを入れておけば、ヒストリに保存されない)
unsetopt hist_verify         # ヒストリを呼び出してから実行する間に一旦編集可能を止める
setopt hist_reduce_blanks    # 余分な空白は詰めて記録
setopt hist_save_no_dups     # ヒストリファイルに書き出すときに、古いコマンドと同じものは無視する。
setopt hist_no_store         # historyコマンドは履歴に登録しない
setopt hist_expand           # 補完時にヒストリを自動的に展開

# 全履歴の一覧を出力する
function history-all { history -E 1 }

##========================================================##
##=================== プロンプトの設定 ===================##
##========================================================##
autoload -U promptinit ; promptinit
autoload -U colors     ; colors
# プロンプトテーマを表示するコマンド
# prompt -l
# 基本のプロンプト

#PROMPT="%{$reset_color%}$ "

#PROMPT="%F{cyan}%n%f%# "

PROMPT="
%F{yellow}%d%f :: (%F{cyan}%n%f@%F{cyan}%M%f): 
%# "

#PROMPT="%{$reset_color%}%{$fg[green]%}$USER%{$reset_color%}@%{$fg[cyan]%}%m%{$reset_color%}$PROMPT"
#RPROMPT="%{$fg[green]%}[%*]%{$reset_color%}"
#RPROMPT="[%F{green}%B%.%b%f@%F{yellow}%U%m%u%f]"


##========================================================##
##================ ディレクトリ移動の設定 ================##
##========================================================##
setopt auto_cd               # ディレクトリのみで移動
setopt auto_pushd            # 普通に cd するときにもディレクトリスタックにそのディレクトリを入れる
setopt pushd_ignore_dups     # ディレクトリスタックに重複する物は古い方を削除
setopt pushd_to_home         # pushd 引数ナシ == pushd $HOME
setopt pushd_silent          # pushd,popdの度にディレクトリスタックの中身を表示しない
# pop command
alias pd='popd'
alias gd='dirs -v; echo -n "select number: ";
read newdir; cd +"$newdir" '

##========================================================##
##====================== 雑多な設定 ======================##
##========================================================##
#setopt AUTOLOGOUT=n          # n分後に自動的にログアウト
setopt no_beep               # コマンド入力エラーでBeepを鳴らさない
#setopt beep

setopt complete_in_word
setopt extended_glob         # 拡張グロブを有効にする
setopt brace_ccl             # ブレース展開機能を有効にする
setopt equals                # =COMMAND を COMMAND のパス名に展開
setopt numeric_glob_sort     # 数字を数値と解釈してソートする
setopt path_dirs             # コマンド名に / が含まれているとき PATH 中のサブディレクトリを探す
setopt print_eight_bit       # 補完候補リストの日本語を適正表示
setopt auto_name_dirs

unsetopt flow_control        # (shell editor 内で) C-s, C-q を無効にする
setopt no_flow_control       # C-s/C-q によるフロー制御を使わない
setopt hash_cmds             # 各コマンドが実行されるときにパスをハッシュに入れる

#setopt ignore_eof            # C-dでログアウトしない

setopt bsd_echo
setopt no_hup                # ログアウト時にバックグラウンドジョブをkillしない
#setopt no_checkjobs          # ログアウト時にバックグラウンドジョブを確認しない
setopt notify                # バックグラウンドジョブが終了したら(プロンプトの表示を待たずに)すぐに知らせる
setopt long_list_jobs        # 内部コマンド jobs の出力をデフォルトで jobs -L にする

setopt magic_equal_subst     # コマンドラインの引数で --PREFIX=/USR などの = 以降でも補完できる
#setopt mail_warning
setopt multios               # 複数のリダイレクトやパイプなど、必要に応じて TEE や CAT の機能が使われる
setopt short_loops           # FOR, REPEAT, SELECT, IF, FUNCTION などで簡略文法が使えるようになる
#setopt sun_keyboard_hack     # SUNキーボードでの頻出 typo ` をカバーする
setopt always_last_prompt    # カーソル位置は保持したままファイル名一覧を順次その場で表示
setopt cdable_vars sh_word_split

setopt rm_star_wait          # rm * を実行する前に確認
#setopt rm_star_silent        # rm * を実行する前に確認しない
#setopt no_clobber            # リダイレクトで上書きを禁止
unsetopt no_clobber

setopt no_unset              # 未定義変数の使用禁止

#setopt interactive_comments  # コマンド入力中のコメントを認める
#setopt chase_links           # シンボリックリンクはリンク先のパスに変換してから実行
#setopt print_exit_value      # 戻り値が 0 以外の場合終了コードを表示
#setopt single_line_zle       # デフォルトの複数行コマンドライン編集ではなく、１行編集モードになる
#setopt xtrace                # コマンドラインがどのように展開され実行されたかを表示する

# less の動作（man less 参照）
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

# タイトルバーの動的変更
precmd() {
[[ -t 1 ]] || return
case $TERM in
sun-cmd) print -Pn "\e]l[%~]\e\\"
 ;;
*xterm*|rxvt|(dt|k|E)term) print -Pn "\e]2;[%~]\a"
 ;;
esac
}

# Global alias
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

# HTMLファイルに張り付け用の、タブ、空白、< > の変換コマンド
alias htmlconv='sed -e "s/</\&lt;/g;s/>/\&gt;/g;s/\t/\&nbsp;\&nbsp;\&nbsp;\&nbsp;/g;s/\s/\&nbsp;/g" '

alias l='ls -CFa --color'
alias ll='ls -l -F --color'
alias lla='ls -l -a -F --color'

# Acroread の Completion が遅い問題を回避
_acroread_version='7.0.9'
alias close='screen -D'


# coloring for ls
eval $(dircolors -b ~/.dir_colors)


# export
export PATH="$PATH":/home/satoru/bin
alias pse='ps aux | grep'
