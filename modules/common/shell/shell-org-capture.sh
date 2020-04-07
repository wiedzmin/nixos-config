TEMPLATE="$1"
if [[ ! -n $TEMPLATE ]]; then
  exit 1
fi
TITLE="$*"
if [[ -n $TMUX ]]; then
  TITLE=$(tmux display-message -p '#S')
  tmux send -X copy-pipe-and-cancel "xsel -i --primary"
fi

if [[ -n $TITLE ]]; then
  emacsclient -n "org-protocol://capture?template=$TEMPLATE&title=$TITLE"
else
  emacsclient -n "org-protocol://capture?template=$TEMPLATE"
fi
