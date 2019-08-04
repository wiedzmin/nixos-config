{ bash, emacs, tmux, xsel, ... }:
''
    #!${bash}/bin/bash

    TEMPLATE="$1"
    if [[ ! -n $TEMPLATE ]]
    then
        exit 1
    fi
    TITLE="$*"
    if [[ -n $TMUX ]]
    then
        TITLE=$(${tmux}/bin/tmux display-message -p '#S')
        ${tmux}/bin/tmux send -X copy-pipe-and-cancel "${xsel}/bin/xsel -i --primary"
    fi

    if [[ -n $TITLE ]]
    then
        ${emacs}/bin/emacsclient -n "org-protocol://capture?template=$TEMPLATE&title=$TITLE"
    else
        ${emacs}/bin/emacsclient -n "org-protocol://capture?template=$TEMPLATE"
    fi
''
