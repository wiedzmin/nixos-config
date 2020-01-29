# FIXME: check if there is actually something in clipboard
ix_url=$(@xselBinary@ -ob | @ixBinary@)
echo -n "$ix_url" | @xselBinary@ -ib
