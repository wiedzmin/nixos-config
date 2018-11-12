{config, pkgs, ...}:

{
    config = {
        nixpkgs.config.packageOverrides = super: {
            is-git-repo = pkgs.writeShellScriptBin "is-git-repo" ''
                 ${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null
            '';
            git-fetch-batch = pkgs.writeShellScriptBin "git-fetch-batch" ''
                set -euo pipefail
                BASE_PATH=$1
                if [[ ! -n $BASE_PATH ]]
                then
                    exit 1
                fi

                set -x

                for item in $(find $BASE_PATH -type d -name ".git")
                do
                    cd $item/..
                    echo "Processing $(basename `pwd`)..."
                    REMOTES=$(${pkgs.git}/bin/git remote)
                    if [[ "origin" =~ $REMOTES ]]; then
                        ${pkgs.git}/bin/git fetch origin &
                        wait $!
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
                        GIT_REPOS=$(find $BASE_PATH -type d -name ".git")
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

                for item in $GIT_REPOS
                do
                    echo "$item"
                    if [[ $ADJUST_GIT_REPOS -eq 1 ]]; then
                         cd $item/..
                    else
                         cd $item
                    fi
                    echo "Processing $(basename `pwd`)..."
                    ${pkgs.gitAndTools.stgit}/bin/stg init
                    ${pkgs.gitAndTools.stgit}/bin/stg repair

                    if [[ ! -z $(${pkgs.git}/bin/git status --porcelain) ]]; then
                        ${pkgs.git}/bin/git add .
                        PATCH_DESC="WIP $(date -R)"
                        ${pkgs.gitAndTools.stgit}/bin/stg new -m "$PATCH_DESC"
                        ${pkgs.gitAndTools.stgit}/bin/stg refresh
                    fi
                done
            '';
            bitbucket_team_contributor_repos = pkgs.writeShellScriptBin "bitbucket_team_contributor_repos" ''
                # TODO: think of keyword args or likewise
                TEAM=$1
                PROJECTS_EXCLUDE=$2
                CREDENTIALS=$(${pkgs.pass_curl_helper}/bin/pass_curl_helper alex3rd/webservices/social/programming/bitbucket.com.web)
                RESULT=$(${pkgs.curl}/bin/curl -s -u \
                       $CREDENTIALS "https://api.bitbucket.org/2.0/repositories?role=contributor&pagelen=200" | \
                       ${pkgs.jq}/bin/jq -r '.values[] | select(.project.name != null) | "\(.links.clone[0].href)~\(.project.name)"')
                if [[ ! -z $TEAM ]]; then
                    RESULT=$(printf "%s\n" $RESULT | grep $TEAM)
                fi
                if [[ ! -z $PROJECTS_EXCLUDE ]]; then
                    GREP_CLAUSES=$(echo $PROJECTS_EXCLUDE | ${pkgs.gnused}/bin/sed "s/,/\|/g")
                    RESULT=$(printf "%s\n" $RESULT | grep -i -v -E $GREP_CLAUSES)
                fi
                for repo in $RESULT; do
                    echo $repo | ${pkgs.coreutils}/bin/cut -f1 -d~
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
                LOCAL=$(git rev-parse @)
                REMOTE=$(git rev-parse "$UPSTREAM")
                BASE=$(git merge-base @ "$UPSTREAM")

                if [ $LOCAL = $REMOTE ]; then
                    echo ''${UPTODATE_MESSAGE:-"Up-to-date"}
                elif [ $LOCAL = $BASE ]; then
                    echo ''${NEEDFETCH_MESSAGE:-"Need to fetch"}
                elif [ $REMOTE = $BASE ]; then
                    echo ''${NEEDPUSH_MESSAGE:-"Need to push"}
                else
                    echo ''${DIVERGED_MESSAGE:-"Diverged"}
                fi
           '';
        };
    };
}
