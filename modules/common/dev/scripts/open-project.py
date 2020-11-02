import json
import re
import sys
import time

import redis

from pystdlib.uishim import get_selection
from pystdlib import shell_cmd
from pystdlib.xlib import switch_named_desktop



r = redis.Redis(host='localhost', port=6379, db=0)
bookmarks = json.loads(r.get("nav/bookmarks"))

if not len(bookmarks):
    notify("[bookmarks]", "No entries", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

selected_bookmark = get_selection(bookmarks.keys(), "", lines=15, font="@wmFontDmenu@")
if selected_bookmark:
    bookmark_path = bookmarks[selected_bookmark]
    elisp_cmd = f'(dired "{bookmark_path}")'
    emacs_cmd = f'emacsclient -c -s /run/user/1000/emacs/server -e \'{elisp_cmd}\' &' # TODO: make SPOT for socket path
    shell_cmd(emacs_cmd, oneshot=True)
