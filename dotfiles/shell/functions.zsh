mkd(){ mkdir "$1" && cd "$1"; }
rmd(){ local P="`pwd`"; cd .. && rmdir "$P" || cd "$P"; }
findname() { pattern=$1; find . -name "${pattern}" }
find_in_files() { fnpattern=$1; grpattern=$2; find . -name "${fnpattern}" -exec grep "${grpattern}" -n {} + }

dot() {
    if [[ $LBUFFER = *.. ]]; then
        LBUFFER+=/..
    else
        LBUFFER+=.
    fi
}

# edit file with root privs
function eed() {
    emacsclient -c -a emacs "/sudo:root@localhost:$1"
}

# see https://github.com/junegunn/fzf/wiki/examples
# fbr - checkout git branch (including remote branches)
fbr() {
    local branches branch
    branches=$(git branch --all | grep -v HEAD) &&
        branch=$(echo "$branches" |
                     fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
        if [ "x$branch" != "x" ]
        then
            git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
        fi
}

# fco - checkout git branch/tag
fco() {
    local tags branches target
    tags=$(
        git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return
    branches=$(
        git branch --all | grep -v HEAD             |
            sed "s/.* //"    | sed "s#remotes/[^/]*/##" |
            sort -u          | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
    target=$(
        (echo "$tags"; echo "$branches") |
            fzf-tmux -- --no-hscroll --ansi +m -d "\t" -n 2) || return
    if [ "x$target" != "x" ]
    then
        git checkout $(echo "$target" | awk '{print $2}')
    fi
}

# fcoc - checkout git commit
fcoc() {
    local commits commit
    commits=$(git log --pretty=oneline --abbrev-commit --reverse)
    commit=$(echo "$commits" | fzf --tac +s +m -e)
    if [ "x$commit" != "x" ]
    then
        git checkout $(echo "$commit" | sed "s/ .*//")
    fi
}

# fshow - git commit browser
fshow() {
    local out sha q
    while out=$(
            git log --decorate=short --graph --oneline --color=always |
                fzf --ansi --multi --no-sort --reverse --query="$q" --print-query); do
        q=$(head -1 <<< "$out")
        while read sha; do
            [ -n "$sha" ] && git show --color=always $sha | less -R
        done < <(sed '1d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" | awk '{print $1}')
    done
}

# fkill - kill process
fkill() {
    pid=$(ps -ef | sed 1d | fzf | awk '{print $2}')

    if [ "x$pid" != "x" ]
    then
        kill -${1:-9} $pid
    fi
}

# fj - changing directory with fasd
fj() {
    local dir
    dir=$(fasd -Rdl | peco --initial-filter SmartCase) && cd "$dir"
}

__fzf_use_tmux__() {
    [ -n "$TMUX_PANE" ] && [ "${FZF_TMUX:-0}" != 0 ] && [ ${LINES:-40} -gt 15 ]
}

__fzfcmd() {
    __fzf_use_tmux__ &&
        echo "fzf-tmux -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
}

fzf-history-widget() {
    local selected num
    setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
    selected=( $(fc -l 1 |
                     FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS --tac -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS --query=${(q)LBUFFER} +m" $(__fzfcmd)) )
    local ret=$?
    if [ -n "$selected" ]; then
        num=$selected[1]
        if [ -n "$num" ]; then
            zle vi-fetch-history -n $num
        fi
    fi
    zle redisplay
    typeset -f zle-line-init >/dev/null && zle zle-line-init
    return $ret
}

# fe [FUZZY PATTERN] - Open the selected file with the default editor
# - Bypass fuzzy finder if there's only one match (--select-1)
# - Exit if there's no match (--exit-0)
fe() {
    local openfile
    openfile=$(rg -g "*" --files | $(__fzfcmd))
    [[ -n "$openfile" ]] && ${EDITOR:-vim} "${openfile}"
}

#--------------------------------------------------------------------------
# A Cleaner Print of your current IP
#--------------------------------------------------------------------------
function ip() {
    ifconfig eth0 | grep 'inet ' | sed -e 's/:/ /' | awk '{print "eth0 (IPv4): " $2 " " $3 " " $4 " " $5 " " $6}'
    ifconfig wlan0 | grep 'inet ' | sed -e 's/ / /'| awk '{print "wlan0 (IPv4): " $2 " " $3 " " $4 " " $5 " " $6}'
}

safeload () {
    [ -f $1 ] && source $1
}

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
