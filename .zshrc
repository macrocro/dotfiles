# PATH=/usr/local/opt:$PATH
PATH=/usr/local/bin:$PATH
disable r
alias r="rails"
alias ll="ls -alG"
alias ls="ls -G"
alias grep="grep --color=auto"
alias e="emacsclient -t"

## 補完時に大小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' menu select=1

autoload -U compinit && compinit

## options
setopt BASH_AUTO_LIST
setopt LIST_AMBIGUOUS
setopt AUTO_PUSHD

## history
HISTFILE="$HOME/.zsh_history"
HISTSIZE=16384
SAVEHIST=16384
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt share_history


# PROMPT

#PS1="[@${HOST%%.*} %1~]%(!.#.$) " # この辺は好み
case "$TERM" in
    xterm*|kterm*|rxvt*)
    PROMPT=$(print "%B%{\e[34m%}%m:(´・ω・｀):%(5~,%-2~/.../%2~,%~)%{\e[33m%}%# %b")
    PROMPT=$(print "%{\e]2;%n@%m:(´・ω・｀): %~\7%}$PROMPT") # title bar
    ;;
    *)
    PROMPT='%m:%c%# '
    ;;
esac

RPROMPT="%T"                      # 右側に時間を表示する

setopt transient_rprompt          # 右側まで入力がきたら時間を消す

setopt prompt_subst               # 便利なプロント

bindkey -e                        # emacsライクなキーバインド



#export LANG=ja_JP.UTF-8           # 日本語環境
##emacs fatal error 11 回避
alias edit='DISPLAY=${DISPLAY/*:/:} emacs'


export EDITOR='emacs'               # エディタはemacs
#export EDITOR=vim               # エディタはemacs



autoload -U compinit              # 強力な補完機能

compinit -u                       # このあたりを使わないとzsh使ってる意味なし

setopt autopushd          # cdの履歴を表示

setopt pushd_ignore_dups          # 同ディレクトリを履歴に追加しない

setopt auto_cd                    # 自動的にディレクトリ移動

setopt list_packed   # リストを詰めて表示

setopt list_types                 # 補完一覧ファイル種別表示



# 履歴

HISTFILE=~/.zsh_history           # historyファイル

HISTSIZE=10000                    # ファイルサイズ

SAVEHIST=10000                    # saveする量

setopt hist_ignore_dups           # 重複を記録しない

setopt hist_reduce_blanks         # スペース排除

setopt share_history              # 履歴ファイルを共有

setopt EXTENDED_HISTORY           # zshの開始終了を記録



# history 操作まわり

autoload history-search-end

zle -N history-beginning-search-backward-end history-search-end

zle -N history-beginning-search-forward-end history-search-end

bindkey "^P" history-beginning-search-backward-end

bindkey "^N" history-beginning-search-forward-end

# ビープ音を消す
setopt nolistbeep

zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'


[ -f ~/.zshrc.include ] && source ~/.zshrc.include # 設定ファイルのinclude

eval "$(rbenv init - zsh)"

#octave用
#export GNUTERM=x11

export PGHOST=localhost
export PGDATA=/usr/local/var/postgres

# emacs daemon
if ps aux | grep -e "emacs --daemon$" >/dev/null 2>&1; then
else
    emacs --daemon
fi

# tmux auto start
if [ -z "$TMUX" -a -z "$STY" ]; then
    if type tmux >/dev/null 2>&1; then
        if tmux has-session && tmux list-sessions | grep -qE '.*]'; then
            tmux attach && echo "tmux attached session "
        else
            tmux new-session && echo "tmux created new session"
        fi
    fi
fi
