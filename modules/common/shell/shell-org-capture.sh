TEMPLATE="$1"
if [[ ! -n $TEMPLATE ]]
then
    exit 1
fi
TITLE="$*"
if [[ -n $TMUX ]]
then
    TITLE=$(@tmuxBinary@ display-message -p '#S')
    @tmuxBinary@ send -X copy-pipe-and-cancel "@xselBinary@ -i --primary"
fi

if [[ -n $TITLE ]]
then
    @emacsclientBinary@ -n "org-protocol://capture?template=$TEMPLATE&title=$TITLE"
else
    @emacsclientBinary@ -n "org-protocol://capture?template=$TEMPLATE"
fi
