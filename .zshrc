# env
if [[ $OSTYPE =~ darwin* ]]; then
    export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
    export PATH="$HOME/bin:/usr/local/bin:$PATH"
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
fi

# プロンプト設定
setopt prompt_subst # プロンプトを出すたびに再読み込み
autoload -Uz add-zsh-hook
autoload -Uz colors # fg[color]を使えるようにする
colors
# 右プロンプト
autoload -Uz vcs_info # vcs 情報を取得する関数を読み込む
zstyle ':vcs_info:*' max-exports 3
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats '[%b]' '%c%u %m'
zstyle ':vcs_info:git:*' actionformats '[%b]' '%c%u %m' '<!%a>'
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "+" # %c
zstyle ':vcs_info:git:*' unstagedstr "-" # %u
# hooks 設定
zstyle ':vcs_info:git+set-message:*' hooks \
    git-hook-begin \
    git-untracked \
    git-push-status \
    git-nomarge-branch \
    git-stash-count
# hook の最初の関数
# git の作業コピーのあるディレクトリのみこの関数を呼び出す
function +vi-git-hook-begin() {
    if [[ $(command git rev-parse --is-inside-work-tree 2> /dev/null) != 'true' ]]; then
        return 1 # 0以外を返すとそれ以降の関数は呼び出されない
    fi
    return 0
}
# untracked ファイル表示
# untracked ファイルとは, バージョン管理されていないファイル
# %u に ? を表示
function +vi-git-untracked() {
    if [[ "$1" != "1" ]]; then # zstyle formats, actionformats の 2 番目のメッセージのみ対象
        return 0
    fi
    if command git status --porcelain 2> /dev/null \
        | awk '{print $1}' \
        | command grep -F '??' > /dev/null 2>&1; then
        hook_com[unstaged]+='?'
    fi
}
# push していないコミットの件数表示
# リモートリポジトリに push していないコミットの件数を pN という形式で %m に表示する
function +vi-git-push-status() {
    if [[ "$1" != "1" ]]; then
        return 0
    fi
    if [[ "${hook_com[branch]}" != "master" ]]; then # master ブランチ出ない場合は何もしない
        return 0
    fi
    local ahead
    ahead=$(command git rev-list origin/master..master 2>/dev/null \
        | wc -l \
        | tr -d ' ')
    if [[ "$ahead" -gt 0 ]]; then
        hook_com[misc]+="(push:${ahead})"
    fi
}
# マージしていない件数を表示
# master 以外のブランチにいる場合に, 現在のブランチ上でまだ master にマージしていないコミットの件数を mN という形式で %m に表示
function +vi-git-nomerge-branch() {
    if [[ "$1" != "1" ]]; then
        return 0
    fi
    if [[ "${hook_com[branch]}" == "master" ]]; then # master ブランチなら何もしない
        return 0
    fi
    local nomarged
    nomarged=$(command git rev-list master..${hook_com[branch]} 2>/dev/null | wc -l | tr -d ' ')
    if [[ "$nomerged" -gt 0 ]]; then
        hook_com[misc]+="(marge:${nomerged})"
    fi
}
# stash 件数表示
# stash している場合は :SN という形式で %m に表示
function +vi-git-stash-count() {
    if [[ "$1" != "1" ]]; then
        return 0
    fi
    local stash
    stash=$(command git stash list 2>/dev/null | wc -l | tr -d ' ')
    if [[ "${stash}" -gt 0 ]]; then
        hook_com[misc]+="stash:${stash}"
    fi
}

function _update_vcs_info_msg() {
    local -a messages
    local prompt
    LANG=en_US.UTF-8 vcs_info
    if [[ -z ${vcs_info_msg_0_} ]]; then
        prompt=""
    else
        [[ -n "$vcs_info_msg_0_" ]] && messages+=( "%F{cyan}${vcs_info_msg_0_}%f" )
        [[ -n "$vcs_info_msg_1_" ]] && messages+=( "%F{yellow}${vcs_info_msg_1_}%f" )
        [[ -n "$vcs_info_msg_2_" ]] && messages+=( "%F{red}${vcs_info_msg_2_}%f" )
        prompt="${(j: :)messages}"
    fi
    RPROMPT="$prompt"
}
add-zsh-hook precmd _update_vcs_info_msg
# 左プロンプト
function prompt_set() {
    #eval `resize`
    local p_rhst=""
    if [[ -n "${REMOTEHOST}${SSH_CONNECTION}" ]]; then
        local rhost=`who am i | sed 's/.*(\(.*\)).*/\1/'`
        rhost=${rhost#localhost:}
        rhost=${rhost%%.*}
        p_rhst="%F{yelow}($rhost)%f"
    fi
    local p_cdir="===%F{cyan}<%~>[%D{%m/%d %T}]%f"
    local p_info="%(!,%F{magenta},%F{green})%n@%m%f"
    local p_mark="%B%(?,%F{gray},%F{red})%(!,##,->)%f%b"
    local p_dir_num=`print -n -P -- "$p_cdir" | sed -e $'s/\e\[[0-9;]*m//g' | wc -m | sed -e 's/ //g'`
    local p_rest_len=`expr $COLUMNS - $p_dir_num - 1`
    local p_line=${(l:${p_rest_len}::=:)}

    PROMPT="$p_cdir$p_line"$'\n'"$p_rhst$p_info $p_mark "
}
add-zsh-hook precmd prompt_set

# ls の色設定(solarized)
eval `dircolors ~/.dircolors`
export ZLS_COLORS=$LS_COLORS

# zsh 補完時の色設定(solarized)
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# 大文字小文字を区別しない(大文字を入力すると区別する)
zstyle 'completion:*' matcher-list 'm:{a-z}={A-Z}'

# 補完後更に <Tab> を押すことで, 候補を選択できる
zstyle ':completion:*:default' menu slect=1

# vi ライクな操作
bindkey -v

# 自動補完
autoload -U compinit
compinit

# 履歴設定
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# ファイル生成を強化
setopt extended_glob

# rm * の時確認する
setopt rm_star_wait

# スペルミスを Dvorak 配列で判断
setopt dvorak

# cd を入力しなくても良い
setopt auto_cd

# ディレクトリスタックを有効, すでに存在するpathは登録しない
setopt auto_pushd
setopt pushd_ignore_dups

# 履歴にあるコマンドは古いものから削除
setopt hist_ignore_all_dups

# 履歴を端末間で共有
setopt share_history

# 単語の一部として扱う記号の指定
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# alias
# mac用
#local -a cmds
#cmds=(ls cp mv ln cat touch seq rmdir rm mkdir mkfifo)
#cmds+=(wc uniq sort tee tail head join cut)
#cmds+=(chown chmod chgrp ps sleep nice)
#cmds+=(xargs find)
#cmds+=(grep fgrep egrep)
#
#for cmd in $cmds; do
#    alias $cmd=`whence "g$cmd" || whence "$cmd"`
#done
alias ls='ls --color=auto -F'
alias ll='ls -l'
alias la='ls -a'
