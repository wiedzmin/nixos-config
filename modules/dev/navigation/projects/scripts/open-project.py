import json
import re
import sys
import time

import redis

from pystdlib import shell_cmd
from pystdlib.shell import tmux_create_window
from pystdlib.uishim import get_selection, notify, URGENCY_CRITICAL
from pystdlib.xlib import switch_named_desktop


r = redis.Redis(host='localhost', port=6379, db=0)
bookmarks = json.loads(r.get("nav/bookmarks"))

if not len(bookmarks):
    notify("[bookmarks]", "No entries", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

bookmark = get_selection(bookmarks.keys(), "", lines=15, font="@wmFontDmenu@")
if bookmark:
    meta = bookmarks[bookmark]
    path = meta["path"]
    shell = meta.get("shell", None)
    if shell:
        tmux_session = meta.get("tmux", "@tmuxDefaultSession@")
        tmux_create_window(None, tmux_session, window_title=path.split("/")[-1], attach=True, start_directory=path)
    elisp_cmd = f'(dired "{path}")'
    emacs_cmd = f'emacsclient -c -s /run/user/1000/emacs/server -e \'{elisp_cmd}\' &' # TODO: make SPOT for socket path
    shell_cmd(emacs_cmd, oneshot=True)
