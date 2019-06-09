{config, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
let
    nixDuBasedir = "/tmp";
    nixDuFilename = "nix-du";
    nixDuFileFormat = "svg";
    nixDuSizeThreshold = "500MB";
    warningsOrgFile = "$HOME/warnings.org";
in
{
    config = {
        environment.systemPackages = with pkgs; [
            gen-nix-du
        ];
        nixpkgs.config.packageOverrides = super: {
            is-git-repo = pkgs.writeShellScriptBin "is-git-repo" ''
                ${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null
            '';
            watch_git_remote_status = pkgs.writeShellScriptBin "watch_git_remote_status" ''
                GIT_REPO=''${1:-'.'}
                cd $GIT_REPO
                if [ -z "$(${pkgs.is-git-repo}/bin/is-git-repo)" ]; then
                    echo "Not a git repo"
                    exit 1
                fi
                UPSTREAM=''${2:-'@{u}'}
                LOCAL=$(${pkgs.git}/bin/git rev-parse @)
                REMOTE=$(${pkgs.git}/bin/git rev-parse "$UPSTREAM")
                BASE=$(${pkgs.git}/bin/git merge-base @ "$UPSTREAM")

                if [ "$LOCAL" == "$REMOTE" ]; then
                    echo ''${UPTODATE_MESSAGE:-"Up-to-date"}
                elif [ "$LOCAL" == "$BASE" ]; then
                    echo ''${NEEDFETCH_MESSAGE:-"Need to fetch"}
                elif [ "$REMOTE" == "$BASE" ]; then
                    echo ''${NEEDPUSH_MESSAGE:-"Need to push"}
                else
                    echo ''${DIVERGED_MESSAGE:-"Diverged"}
                fi
            '';
            # TODO: add secrets pre-commit checking
            git_hooks_lib = pkgs.writeShellScriptBin "git_hooks_lib" ''
                WIP_RE=wip

                execute_hook_items() {
                    hook=$1
                    hooks_basedir=$(pwd)/${gitRepoHooks}

                    if [ -z $hook ]; then
                        echo "no hook provided, exiting"
                        exit 1
                    fi

                    if [ ! -d "$hooks_basedir" ]; then
                        # repo hooks were not initialized, simply exit with success
                        exit 0
                    fi

                    IS_CLEAN=true
                    TARGET_DIR="$hooks_basedir/$hook"

                    shopt -s execfail

                    for TARGET_PATH in $(${pkgs.fd}/bin/fd . -L --max-depth 1 --type f $TARGET_DIR | ${pkgs.coreutils}/bin/sort)
                    do
                        if [ -x "$TARGET_PATH" ]; then # Run as an executable file
                            "$TARGET_PATH" "$@"
                        elif [ -f "$TARGET_PATH" ]; then  # Run as a Shell script
                            ${pkgs.bash}/bin/bash "$TARGET_PATH" "$@"
                        fi
                        exitcode=$?
                        if (( exitcode != 0 )); then
                            IS_CLEAN=false
                            ${if shortCircuitGitHooks then "return $exitcode" else ""}
                        fi
                    done
                    if [[ "$IS_CLEAN" == "false" ]]; then
                        return 1;
                    fi
                    return 0;
                }

                check_for_wip() {
                    RESULTS=$(${pkgs.git}/bin/git shortlog "@{u}.." | ${pkgs.gnugrep}/bin/grep -w $WIP_RE);
                    if [[ ! -z "$RESULTS" ]]; then
                        echo "Found commits with stop snippets:"
                        echo "$RESULTS"
                        return 1;
                    fi
                    return 0;
                }

                check_for_secrets() {
                    RESULTS=$(${pkgs.gitAndTools.git-secrets}/bin/git-secrets --scan --cached 2>&1)
                    if [[ -z "$RESULTS" ]]; then
                        return 0;
                    fi
                    echo "Found secret snippets:"
                    echo "$RESULTS"
                    return 1;
                }

                check_org_delete_treshold() {
                    CURRENT_REV=`${pkgs.git}/bin/git rev-parse HEAD`
                    PREVIOUS_REV=`${pkgs.git}/bin/git rev-parse HEAD^1`

                    OUTFILE="${warningsOrgFile}"
                    THRESHOLD=250

                    MESSAGE="** commit ''${CURRENT_REV} deleted more than ''${THRESHOLD} lines in a file!"

                    DETAILS="#+BEGIN_SRC sh :results output
                    cd ${builtins.dirOf warningsOrgFile}
                    echo \"commit ''${CURRENT_REV}\"
                    ${pkgs.git}/bin/git diff --stat \"''${PREVIOUS_REV}\" \"''${CURRENT_REV}\"
                    #+END_SRC"

                    ${pkgs.git}/bin/git diff --numstat "''${PREVIOUS_REV}" "''${CURRENT_REV}" | \
                      cut -f 2 | \
                      while read line
                        do test "$line" -gt "''${THRESHOLD}" && \
                          echo "''${MESSAGE}\n<`date '+%Y-%m-%d %H:%M'` +1d>\n\n''${DETAILS}\n" >> \
                          "''${OUTFILE}"; \
                        done
                }
            '';
            gen-nix-du = pkgs.writeShellScriptBin "gen-nix-du" ''
                set -eu

                ${pkgs.nix-du}/bin/nix-du -s ${nixDuSizeThreshold} | ${pkgs.graphviz}/bin/dot -T${nixDuFileFormat} > ${nixDuBasedir}/${nixDuFilename}.${nixDuFileFormat}
            '';
            watch_nixpkgs_updates = pkgs.writeShellScriptBin "watch_nixpkgs_updates" ''
                if [ ! -z "$(${pkgs.watch_git_remote_status}/bin/watch_git_remote_status /etc/nixos/pkgs/nixpkgs-channels | grep $NEEDFETCH_MESSAGE)" ]; then
                    ${pkgs.dunst}/bin/dunstify -t 30000 "Nixpkgs updated, consider install!"
                fi
            '';
            watch_home_manager_updates = pkgs.writeShellScriptBin "watch_home_manager_updates" ''
                if [ ! -z "$(${pkgs.watch_git_remote_status}/bin/watch_git_remote_status /etc/nixos/pkgs/home-manager | grep $NEEDFETCH_MESSAGE)" ]; then
                    ${pkgs.dunst}/bin/dunstify -t 30000 "Home-manager updated, consider install!"
                fi
            '';
            show_nixpkgs_updates = pkgs.writeShellScriptBin "show_nixpkgs_updates" ''
                echo "$(${pkgs.watch_git_remote_status}/bin/watch_git_remote_status /etc/nixos/pkgs/nixpkgs-channels)" > /tmp/nixpkgs-channels-git-status
                ${pkgs.yad}/bin/yad --filename /tmp/nixpkgs-channels-git-status --text-info
                rm /tmp/nixpkgs-channels-git-status
            '';
            show_home_manager_updates = pkgs.writeShellScriptBin "show_home_manager_updates" ''
                echo "$(${pkgs.watch_git_remote_status}/bin/watch_git_remote_status /etc/nixos/pkgs/home-manager)" > /tmp/home-manager-git-status
                ${pkgs.yad}/bin/yad --filename /tmp/home-manager-git-status --text-info
                rm /tmp/home-manager-git-status
            '';
            show_current_system_hash = pkgs.writeShellScriptBin "show_current_system_hash" ''
                current_system_commit_hash=`${pkgs.coreutils}/bin/readlink -f /run/current-system | ${pkgs.coreutils}/bin/cut -f4 -d.`
                cd ${nixpkgsFullPath}
                nixpkgs_current_branch=$(${pkgs.git}/bin/git symbolic-ref --short HEAD)
                cd ${homeManagerFullPath}
                hm_current_branch=$(${pkgs.git}/bin/git symbolic-ref --short HEAD)
                hm_current_hash=$(${pkgs.git}/bin/git rev-parse --short HEAD)
                ${pkgs.dunst}/bin/dunstify -t 15000 "nixpkgs: $current_system_commit_hash/$nixpkgs_current_branch
                HM: $hm_current_hash/$hm_current_branch"
            '';
            systemctl-status = pkgs.writeShellScriptBin "systemctl-status" ''
                if [ $# -le 1 ]; then
                    echo -e ""
                else
                    if [[ $# == 2 ]]; then
                        if [[ $2 =~ ^\[ ]]; then
                            STATUS=`${pkgs.systemd}/bin/systemctl status $1 | ${pkgs.gawk}/bin/awk 'NR==3 {print $2}'`
                        else
                            echo -e ""
                        fi
                    elif [[ $# == 3 ]]; then
                        STATUS=`${pkgs.systemd}/bin/systemctl --user status $1 | ${pkgs.gawk}/bin/awk 'NR==3 {print $2}'`
                    fi
                    if [ $STATUS == "inactive" ]
                    then
                        echo -e ""
                    else
                        if [ -z "$2" ]
                        then
                            echo -e "[*]"
                        else
                            echo -e $2
                        fi
                    fi
                fi
            '';
        };
    };
}
