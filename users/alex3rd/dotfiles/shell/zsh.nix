{config, pkgs, lib, ...}:

let
    editors = "emacsclient vim vi";
in
{
    home-manager.users.alex3rd = {
        home.file = {
            ".zsh/functions.zsh".text = ''
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
                    if [[ ! -z $(${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null) ]]; then
                        branches=$(${pkgs.git}/bin/git branch --all -vv) &&
                            branch=$(echo "$branches" | ${pkgs.fzf}/bin/fzf-tmux -- \
                                          --ansi -d $(( 2 + $(wc -l <<< "$branches") )) +m) || return
                            if [ "x$branch" != "x" ]
                            then
                                ${pkgs.git}/bin/git checkout $(echo "$branch" | ${pkgs.gawk}/bin/awk '{print $1}' |
                                ${pkgs.gnused}/bin/sed "s/.* //" | ${pkgs.gnused}/bin/sed "s#remotes/[^/]*/##")
                            fi
                    fi
                } # TODO: make it not asking for extra manual CR in the end somehow

                # fco - checkout git branch/tag
                fco() {
                    local tags branches target
                    if [[ ! -z $(${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null) ]]; then
                        tags=$(${pkgs.git}/bin/git tag | ${pkgs.gawk}/bin/awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') \
                             || return
                        branches=$(${pkgs.git}/bin/git branch --all | grep -v HEAD | ${pkgs.gnused}/bin/sed "s/.* //" |
                                   ${pkgs.gnused}/bin/sed "s#remotes/[^/]*/##" | sort -u |
                                   ${pkgs.gawk}/bin/awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
                        target=$((echo "$tags"; echo "$branches") | ${pkgs.fzf}/bin/fzf-tmux -- \
                               --no-hscroll --ansi +m -d "\t" -n 2) || return
                        if [ "x$target" != "x" ]
                        then
                            ${pkgs.git}/bin/git checkout $(echo "$target" | ${pkgs.gawk}/bin/awk '{print $2}')
                        fi
                    fi
                }

                # fcoc - checkout git commit
                fcoc() {
                    local commits commit
                    if [[ ! -z $(${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null) ]]; then
                        commits=$(${pkgs.git}/bin/git log --pretty=oneline --abbrev-commit --reverse)
                        commit=$(echo "$commits" | ${pkgs.fzf}/bin/fzf --tac +s +m -e)
                        if [ "x$commit" != "x" ]
                        then
                            ${pkgs.git}/bin/git checkout $(echo "$commit" | ${pkgs.gnused}/bin/sed "s/ .*//")
                        fi
                    fi
                }

                # fshow - ${pkgs.git}/bin/git commit browser
                fshow() {
                    local out sha q
                    if [[ ! -z $(${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null) ]]; then
                        while out=$(${pkgs.git}/bin/git log --decorate=short --graph --oneline --color=always |
                                    ${pkgs.fzf}/bin/fzf --ansi --multi --no-sort --reverse --query="$q" --print-query);
                                    do
                            q=$(head -1 <<< "$out")
                            while read sha; do
                                [ -n "$sha" ] && ${pkgs.git}/bin/git show --color=always $sha | less -R
                            done < <(${pkgs.gnused}/bin/sed '1d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" |
                            ${pkgs.gawk}/bin/awk '{print $1}')
                        done
                    fi
                }

                # fkill - kill process
                fkill() {
                    pid=$(ps -ef | ${pkgs.gnused}/bin/sed 1d | ${pkgs.fzf}/bin/fzf | ${pkgs.gawk}/bin/awk '{print $2}')

                    if [ "x$pid" != "x" ]
                    then
                        kill -''${1:-9} $pid
                    fi
                }

                # fj - changing directory with fasd
                fj() {
                    local dir
                    dir=$(${pkgs.fasd}/bin/fasd -Rdl | ${pkgs.fzf}/bin/fzf) && cd "$dir"
                }

                __fzf_use_tmux__() {
                    [ -n "$TMUX_PANE" ] && [ "''${FZF_TMUX:-0}" != 0 ] && [ ''${LINES:-40} -gt 15 ]
                }

                __fzfcmd() {
                    __fzf_use_tmux__ &&
                        echo "${pkgs.fzf}/bin/fzf-tmux -d''${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
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
                    ifconfig eth0 | grep 'inet ' | ${pkgs.gnused}/bin/sed -e 's/:/ /' | ${pkgs.gawk}/bin/awk '{print "eth0 (IPv4): " $2 " " $3 " " $4 " " $5 " " $6}'
                    ifconfig wlan0 | grep 'inet ' | ${pkgs.gnused}/bin/sed -e 's/ / /'| ${pkgs.gawk}/bin/awk '{print "wlan0 (IPv4): " $2 " " $3 " " $4 " " $5 " " $6}'
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
                #!${pkgs.zsh}/bin/zsh

                BIN_DIRS=(
                    ${config.users.extraUsers.alex3rd.home}/scripts
                    ${config.users.extraUsers.alex3rd.home}/tools/bin
                    ${config.users.extraUsers.alex3rd.home}/.local/bin
                )

                ZSH_EXT_DIRS=(
                    ${config.users.extraUsers.alex3rd.home}/.zsh/completion
                )

                export GREP_OPTIONS=--color=auto
                export GREP_COLOR='1;32'
                export SHELL=${pkgs.zsh}/bin/zsh
                export XAUTHORITY=${config.users.extraUsers.alex3rd.home}/.Xauthority
                export FZF_MARKS_FILE=${config.users.extraUsers.alex3rd.home}/.bookmarks
                export GTAGSLIBPATH=${config.users.extraUsers.alex3rd.home}/.gtags/
                export CURRENT_WM=${config.services.xserver.windowManager.default}
                export WORKON_HOME=${config.users.extraUsers.alex3rd.home}/.virtualenvs

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

                for candidate in ${editors} ; do
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
                ${pkgs.any-nix-shell}/bin/any-nix-shell zsh --info-right | source /dev/stdin

                eval "$(${pkgs.fasd}/bin/fasd --init auto)"

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
                FZF_MARKS_FILE = "${config.users.extraUsers.alex3rd.home}/.bookmarks";
                GTAGSLIBPATH = "${config.users.extraUsers.alex3rd.home}/.gtags/";
                WORKON_HOME = "${config.users.extraUsers.alex3rd.home}/.virtualenvs";
                TMUXP_CONFIGDIR = "${config.users.extraUsers.alex3rd.home}/tmuxp";
            };
            shellAliases = {
                dubc = "sudo ${pkgs.findutils}/bin/find . -name __pycache__ -or -name \"*.pyc\" -exec ${pkgs.coreutils}/bin/rm -rf {} + && ${pkgs.docker_compose}/bin/docker-compose up --build";
                TF = "${pkgs.coreutils}/bin/tail -f";
                df = "${pkgs.dfc}/bin/dfc";
                dud = "(setopt globdots; ${pkgs.coreutils}/bin/du -mhs * | ${pkgs.coreutils}/bin/sort -hr)";
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
