# FIXME: check if there is actually something in clipboard
ix_url=$(xsel -ob | ix)
echo -n "$ix_url" | xsel -ib
