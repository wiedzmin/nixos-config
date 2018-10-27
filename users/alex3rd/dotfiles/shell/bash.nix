{config, pkgs, lib, ...}:

let
    editors = "emacsclient vim vi";
in
{
    programs.bash.enableCompletion = true;

    home-manager.users.alex3rd = {
        home.file = {
            ".common_settings".text = ''
                #!${pkgs.bash}/bin/bash

                BIN_DIRS=(
                    ${config.users.extraUsers.alex3rd.home}/scripts
                    ${config.users.extraUsers.alex3rd.home}/tools/bin
                    ${config.users.extraUsers.alex3rd.home}/.local/bin
                )

                export GREP_OPTIONS=--color=auto
                export GREP_COLOR='1;32'
                export SHELL=${pkgs.bash}/bin/bash
                export XAUTHORITY=${config.users.extraUsers.alex3rd.home}/.Xauthority
                export FZF_MARKS_FILE=${config.users.extraUsers.alex3rd.home}/.bookmarks
                export GTAGSLIBPATH=${config.users.extraUsers.alex3rd.home}/.gtags/
                export CURRENT_WM=${config.services.xserver.windowManager.default}
                export WORKON_HOME=${config.users.extraUsers.alex3rd.home}/.virtualenvs
                export PROJECT_HOME=${config.users.extraUsers.alex3rd.home}/workspace/python
                export PROJECTS=${config.users.extraUsers.alex3rd.home}/workspace/foss

                # Remove dupes from 'path', which is array tied to 'PATH'
                # typeset -U path
                for dir in $BIN_DIRS; do
                    if [ -d $dir ] ; then
                        path=($dir "$path[@]")
                    fi
                done

                for candidate in ${editors}; do
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
