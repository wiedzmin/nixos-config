{config, pkgs, lib, ...}:

{
    home-manager.users.alex3rd = {
        home.file = {
            ".zsh/functions.zsh".text = ''
                mkd(){ mkdir "$1" && cd "$1"; }
                rmd(){ local P="`pwd`"; cd .. && rmdir "$P" || cd "$P"; }
                findname() { pattern=$1; find . -name "''${pattern}" }
                find_in_files() { fnpattern=$1; grpattern=$2; find . -name "''${fnpattern}" -exec grep "''${grpattern}" -n {} + }

                dot() {
                    if [[ $LBUFFER = *.. ]]; then
                        LBUFFER+=/..
                    else
                        LBUFFER+=.
                    fi
                }

                # see https://github.com/junegunn/fzf/wiki/examples
                # fbr - checkout git branch (including remote branches)
                fbr() {
                    local branches branch
                    if [[ ! -z $(git rev-parse --git-dir 2> /dev/null) ]]; then
                        branches=$(git branch --all -vv) &&
                            branch=$(echo "$branches" |
                                         fzf-tmux -- --ansi -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
                            if [ "x$branch" != "x" ]
                            then
                                echo
                                git checkout $(echo "$branch" | awk '{print $1}'| sed "s/.* //" | sed "s#remotes/[^/]*/##")
                            fi
                    fi
                } # TODO: make it not asking for extra manual CR in the end somehow

                # fco - checkout git branch/tag
                fco() {
                    local tags branches target
                    if [[ ! -z $(git rev-parse --git-dir 2> /dev/null) ]]; then
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
                    fi
                }

                # fcoc - checkout git commit
                fcoc() {
                    local commits commit
                    if [[ ! -z $(git rev-parse --git-dir 2> /dev/null) ]]; then
                        commits=$(git log --pretty=oneline --abbrev-commit --reverse)
                        commit=$(echo "$commits" | fzf --tac +s +m -e)
                        if [ "x$commit" != "x" ]
                        then
                            git checkout $(echo "$commit" | sed "s/ .*//")
                        fi
                    fi
                }

                # fshow - git commit browser
                fshow() {
                    local out sha q
                    if [[ ! -z $(git rev-parse --git-dir 2> /dev/null) ]]; then
                        while out=$(
                                git log --decorate=short --graph --oneline --color=always |
                                    fzf --ansi --multi --no-sort --reverse --query="$q" --print-query); do
                            q=$(head -1 <<< "$out")
                            while read sha; do
                                [ -n "$sha" ] && git show --color=always $sha | less -R
                            done < <(sed '1d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" | awk '{print $1}')
                        done
                    fi
                }

                # fkill - kill process
                fkill() {
                    pid=$(ps -ef | sed 1d | fzf | awk '{print $2}')

                    if [ "x$pid" != "x" ]
                    then
                        kill -''${1:-9} $pid
                    fi
                }

                # fj - changing directory with fasd
                fj() {
                    local dir
                    dir=$(fasd -Rdl | peco --initial-filter SmartCase) && cd "$dir"
                }

                __fzf_use_tmux__() {
                    [ -n "$TMUX_PANE" ] && [ "''${FZF_TMUX:-0}" != 0 ] && [ ''${LINES:-40} -gt 15 ]
                }

                __fzfcmd() {
                    __fzf_use_tmux__ &&
                        echo "fzf-tmux -d''${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
                }

                fzf-history-widget() {
                    local selected num
                    setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
                    selected=( $(fc -l 1 |
                                     FZF_DEFAULT_OPTS="--height ''${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS --tac -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS --query=''${(q)LBUFFER} +m" $(__fzfcmd)) )
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
                    [[ -n "$openfile" ]] && ''${EDITOR:-vim} "''${openfile}"
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
            '';
            ".common_settings".text = ''
                #!/usr/bin/env zsh

                BIN_DIRS=(
                    /home/alex3rd/scripts
                    $HOME/tools/bin
                    /usr/lib/go/bin
                    $HOME/workspace/gocode/bin
                    $HOME/.local/bin
                )

                ZSH_EXT_DIRS=(
                    $HOME/.zsh/completion
                )

                export GREP_OPTIONS=--color=auto
                export GREP_COLOR='1;32'
                # export SHELL=/bin/bash
                export GOROOT=/usr/lib/go
                export GOPATH=$HOME/workspace/gocode
                export XAUTHORITY=$HOME/.Xauthority
                export FZF_MARKS_FILE=/home/alex3rd/.bookmarks
                export GTAGSLIBPATH=$HOME/.gtags/
                export FZF_ZSH=/usr/share/zsh/site-contrib/fzf.zsh
                export CURRENT_WM=stumpwm
                export WORKON_HOME=$HOME/.virtualenvs
                export PROJECT_HOME=/home/alex3rd/workspace/python
                export PROJECTS=/home/alex3rd/workspace/foss

                # Remove dupes from 'path', which is array tied to 'PATH'
                typeset -U path
                for ((i=1; i<= $#BIN_DIRS; i++)) do
                    for dir in $BIN_DIRS[i]; do
                        if [ -d $dir ] ; then
                            path=($dir "$path[@]")
                        fi
                    done
                done

                # Remove dupes from 'fpath', which is array tied to 'FPATH'
                typeset -U fpath
                for ((i=1; i<= $#ZSH_EXT_DIRS; i++)) do
                    for dir in $ZSH_EXT_DIRS[i]; do
                        if [ -d $dir ] ; then
                            fpath=($dir "$fpath[@]")
                        fi
                    done
                done

                for candidate in emacsclient vim vi ; do
                    if [[ ! -z $(which $candidate) ]]; then
                        export VISUAL=$candidate
                        export EDITOR=$candidate
                        break
                    fi
                done

                export ZBEEP=$'\e[?5h\e[?5l'

                unset GREP_OPTIONS
            '';
        };
        programs.zsh = {
            enable = true;
            oh-my-zsh = {
                enable = true;
                plugins = [
                    "colored-man-pages"
                    "dirpersist"
                    "urltools"
                    "virtualenv"
                    "virtualenvwrapper"
                ];
                theme = "muse";
            };
            enableAutosuggestions = true;
            enableCompletion = true;
            history = {
                size = 10000;
                save = 10000;
                path = ".histfile";
                ignoreDups = true;
                expireDuplicatesFirst = true;
                extended = true;
                share = true;
            };
            initExtra = ''
                PATH=$PATH:${pkgs.autojump}/bin
                . ${pkgs.autojump}/share/autojump/autojump.zsh

                ${pkgs.any-nix-shell}/bin/any-nix-shell zsh --info-right | source /dev/stdin

                if [ `uname -s` = "Linux" ]; then
                    eval `dircolors -b`
                fi

                #setopt BEEP
                setopt APPEND_HISTORY
                setopt BRACECCL
                setopt CORRECT_ALL
                setopt EXTENDED_HISTORY
                setopt HIST_EXPIRE_DUPS_FIRST
                setopt HIST_FIND_NO_DUPS
                setopt HIST_IGNORE_ALL_DUPS
                setopt HIST_IGNORE_DUPS
                setopt HIST_IGNORE_SPACE
                setopt HIST_NO_STORE
                setopt HIST_SAVE_NO_DUPS
                setopt SHARE_HISTORY
                setopt autocd
                setopt correctall
                setopt extended_glob
                setopt inc_append_history
                setopt menucomplete

                autoload -Uz compinit && compinit
                autoload -Uz promptinit && promptinit
                autoload -Uz colors && colors
                autoload -Uz vcs_info
                autoload -U dot
                autoload -U predict-on
                autoload run-help
                zmodload zsh/complist

                source ~/.zsh/functions.zsh

                bindkey "\e[3~" delete-char
                bindkey "^qs" fuzzy-search-and-edit
                bindkey ' ' magic-space # also do history expansion on space
                bindkey -e
                bindkey -r "^g"

                zle -N jump && bindkey "^[xjj" jump
                zle -N dot && bindkey . dot
                zle -N fbr && bindkey "^]bb" fbr
                zle -N fco && bindkey "^]ba" fco
                zle -N fcoc && bindkey "^]cc" fcoc
                zle -N fe && bindkey "^qe" fe
                zle -N fshow && bindkey "^]ll" fshow
                zle -N fzf-history-widget && bindkey "^R" fzf-history-widget
                zle -N predict-off
                zle -N predict-on
            '';
            sessionVariables = {
                GREP_OPTIONS = "--color=auto";
                GREP_COLOR = "1;32";
                FZF_MARKS_FILE = "$HOME/.bookmarks";
                GTAGSLIBPATH = "$HOME/.gtags/";
                WORKON_HOME = "$HOME/.virtualenvs";
                TMUXP_CONFIGDIR = "/etc/nixos/private/tmuxp";
            };
            shellAliases = {
                "-g findgrep" = "find_in_files";
                "-g fnd" = "findname";
                "-g grep" = "grep --color=auto --perl-regexp";
                # ERR = "2>>( sed -ue 's/.*/$fg_bold[red]&$reset_color/' 1>&2 )";
                # dubc = "sudo find . -name '__pycache__' -or -name '*.pyc' -exec rm -rf {} + && docker-compose up --build";
                TF = "tail -f";
                df = "dfc";
                dud = "(setopt globdots; du -mhs * | sort -hr)";
                git = "${pkgs.gitAndTools.hub}/bin/hub";
                gop = "git open";
                jcurl = "curl_jq(){ ${pkgs.curl}/bin/curl $@ | ${pkgs.jq}/bin/jq . }; curl_jq";
                shme = "ssh_whoami(){ ssh `whoami`@$@}; ssh_whoami";
                shroot = "ssh_root(){ ssh root@$@}; ssh_root";

                ls = "${pkgs.exa}/bin/exa -F --color=auto";
                ll = "${pkgs.exa}/bin/exa -l";
                la = "${pkgs.exa}/bin/exa -A";
                cat = "bat";
                cat_raw = "${pkgs.coreutils}/bin/cat";
                zr = ". ~/.zshrc";
            };
            plugins = [ # TODO: bring back other plugins from old system
                {
                    name = "fzf-marks";
                    file = "fzf-marks.plugin.zsh";
                    src = pkgs.fetchgit {
                        url = "https://github.com/urbainvaes/fzf-marks";
                        rev = "1.1";
                        sha256 = "0wfh267kfvyx7vcyqpqv7qgi6vcffxziq5avqddnbkm3z51br0n4";
                    };
                }
                {
                    name = "enhancd";
                    file = "init.sh";
                    src = pkgs.fetchFromGitHub {
                        owner = "b4b4r07";
                        repo = "enhancd";
                        rev = "v2.2.1";
                        sha256 = "0iqa9j09fwm6nj5rpip87x3hnvbbz9w9ajgm6wkrd5fls8fn8i5g";
                    };
                }
            ];
        };
    };
}
