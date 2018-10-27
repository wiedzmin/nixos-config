{config, pkgs, lib, ...}:

{
    programs.bash.enableCompletion = true;

    home-manager.users.alex3rd = {
        home.file = {
            ".common_settings".text = ''
                #!/usr/bin/env bash

                BIN_DIRS=(
                    /home/alex3rd/scripts
                    $HOME/tools/bin
                    /usr/lib/go/bin
                    $HOME/workspace/gocode/bin
                    $HOME/.local/bin
                )

                export GREP_OPTIONS=--color=auto
                export GREP_COLOR='1;32'
                export SHELL=/bin/bash
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
                # typeset -U path
                for dir in $BIN_DIRS; do
                    if [ -d $dir ] ; then
                        path=($dir "$path[@]")
                    fi
                done

                for candidate in emacsclient vim vi; do
                    if [[ ! -z $(which $candidate) ]]; then
                        export VISUAL=$candidate
                        export EDITOR=$candidate
                        break
                    fi
                done

                unset GREP_OPTIONS
            '';
        };
    };
}
