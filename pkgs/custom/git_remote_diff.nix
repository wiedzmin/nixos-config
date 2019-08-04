{ bash, git, ... }:
''
    #!${bash}/bin/bash

    GIT_REPO=''${1:-'.'}
    cd $GIT_REPO
    if [ -z "$(${git}/bin/git rev-parse --git-dir 2> /dev/null)" ]; then
        echo "Not a git repo"
        exit 1
    fi
    UPSTREAM=''${2:-'@{u}'}
    LOCAL=$(${git}/bin/git rev-parse @)
    REMOTE=$(${git}/bin/git rev-parse "$UPSTREAM")
    BASE=$(${git}/bin/git merge-base @ "$UPSTREAM")

    if [ "$LOCAL" == "$REMOTE" ]; then
        echo ''${UPTODATE_MESSAGE:-"Up-to-date"}
    elif [ "$LOCAL" == "$BASE" ]; then
        echo ''${NEEDFETCH_MESSAGE:-"Need to fetch"}
    elif [ "$REMOTE" == "$BASE" ]; then
        echo ''${NEEDPUSH_MESSAGE:-"Need to push"}
    else
        echo ''${DIVERGED_MESSAGE:-"Diverged"}
    fi
''
