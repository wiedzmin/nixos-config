{config, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            is-git-repo = pkgs.writeShellScriptBin "is-git-repo" ''
                 ${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null
            '';
            git-fetch-batch = pkgs.writeShellScriptBin "git-fetch-batch" ''
                BASE_PATH=$1
                if [[ ! -n $BASE_PATH ]]
                then
                    exit 1
                fi

                set -x

                for ITEM in $(${pkgs.findutils}/bin/find $BASE_PATH -type d -name ".git")
                do
                    cd $ITEM/..
                    echo "Processing $(basename `pwd`)..."
                    REMOTES=$(${pkgs.git}/bin/git remote)
                    if [[ "origin" =~ $REMOTES ]]; then
                        SLEEPSEC=2
                        RET=1
                        until [ ''${RET} -eq 0 ]; do
                            ${pkgs.git}/bin/git fetch origin &
                            wait $!
                            RET=$?
                            if [ ''${RET} -ne 0 ]; then
                                echo Failed fetching origin, retrying in $SLEEPSEC seconds...
                                sleep $SLEEPSEC
                                SLEEPSEC="$((SLEEPSEC * 2))"
                            fi
                        done
                        ${pkgs.git}/bin/git rebase --autostash &
                        wait $!
                    fi
                done
            '';
            git-save-wip-batch = pkgs.writeShellScriptBin "git-save-wip-batch" ''
                GIT_REPOS=
                ADJUST_GIT_REPOS=
                case $# in
                    0 )
                        exit 1
                        ;;
                    1 )
                        BASE_PATH=$1
                        GIT_REPOS=$(${pkgs.findutils}/bin/find $BASE_PATH -type d -name ".git")
                        ADJUST_GIT_REPOS=1
                        ;;
                    * )
                        GIT_REPOS=$@
                        ADJUST_GIT_REPOS=0
                        ;;
                esac

                if [[ $(DISPLAY=:0 ${pkgs.xprintidle-ng}/bin/xprintidle-ng) -lt $((3600*1000)) ]]; then
                    exit 0
                fi

                set -x

                for ITEM in $GIT_REPOS
                do
                    echo "$ITEM"
                    if [[ $ADJUST_GIT_REPOS -eq 1 ]]; then
                         cd $ITEM/..
                    else
                         cd $ITEM
                    fi
                    echo "Processing $(basename `pwd`)..."
                    ${pkgs.stgit}/bin/stg init
                    ${pkgs.stgit}/bin/stg repair

                    if [[ ! -z $(${pkgs.git}/bin/git status --porcelain) ]]; then
                        ${pkgs.git}/bin/git add .
                        PATCH_DESC="WIP $(date -R)"
                        ${pkgs.stgit}/bin/stg new -m "$PATCH_DESC"
                        ${pkgs.stgit}/bin/stg refresh
                    fi
                done
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
           bitbucket_team_contributor_repos = pkgs.writeShellScriptBin "bitbucket_team_contributor_repos" ''
                PASS_PATH=$1
                if [ -z "PASS_PATH" ]; then
                    echo "No credentials provided"
                    exit 1
                fi
                TEAM=$2
                PROJECTS_EXCLUDE=$3
                CREDENTIALS=$(${pkgs.pass_curl_helper}/bin/pass_curl_helper $PASS_PATH)
                RESULT=$(${pkgs.curl}/bin/curl -s -u \
                       $CREDENTIALS "https://api.bitbucket.org/2.0/repositories?role=contributor&pagelen=200" | \
                       ${pkgs.jq}/bin/jq -r '.values[] | select(.project.name != null) | "\(.links.clone[0].href)~\(.project.name)"')
                if [[ ! -z $TEAM ]]; then
                    RESULT=$(printf "%s\n" $RESULT | ${pkgs.gnugrep}/bin/grep $TEAM)
                fi
                if [[ ! -z $PROJECTS_EXCLUDE ]]; then
                    GREP_CLAUSES=$(echo $PROJECTS_EXCLUDE | ${pkgs.gnused}/bin/sed "s/,/\|/g")
                    RESULT=$(printf "%s\n" $RESULT | ${pkgs.gnugrep}/bin/grep -i -v -E $GREP_CLAUSES)
                fi
                for REPO in $RESULT; do
                    echo $REPO | ${pkgs.coreutils}/bin/cut -f1 -d~
                done
            '';
           git_hooks_lib = pkgs.writeShellScriptBin "git_hooks_lib" ''
                WIP_TOKEN=wip

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

                    for TARGET_PATH in $(${pkgs.findutils}/bin/find -L $TARGET_DIR -mindepth 1 -maxdepth 1 -type f | ${pkgs.coreutils}/bin/sort)
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
                    RESULTS=$(${pkgs.git}/bin/git shortlog "@{u}.." | ${pkgs.gnugrep}/bin/grep $WIP_TOKEN);
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
            '';
       };
    };
}
