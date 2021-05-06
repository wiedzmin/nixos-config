import argparse
import json
import os
import re
import sys
import time

import redis

from pystdlib import shell_cmd
from pystdlib.shell import tmux_create_window
from pystdlib.systemd import is_systemd_service_active
from pystdlib.uishim import get_selection_rofi, notify, URGENCY_CRITICAL
from pystdlib.xlib import switch_named_desktop


r = redis.Redis(host='localhost', port=6379, db=0)
bookmarks = json.loads(r.get("nav/bookmarks"))

parser = argparse.ArgumentParser(description="Repository overview search")
parser.add_argument('--tmux-session', dest="tmux_session", default="main", type=str, help="Fallback tmux session name")
parser.add_argument('--selector-font', dest="selector_font", type=str, help="Selector font")
args = parser.parse_args()


if not len(bookmarks):
    notify("[bookmarks]", "No entries", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

bookmark = get_selection_rofi(bookmarks.keys(), "")
if bookmark:
    meta = bookmarks[bookmark]
    path = meta["path"]
    shell = meta.get("shell", None)
    if shell:
        tmux_session = meta.get("tmux", args.tmux_session)
        tmux_create_window(None, tmux_session, window_title=path.split("/")[-1], attach=True, start_directory=path)
    if not is_systemd_service_active("emacs", user=True):
        notify("[bookmarks]", "Emacs service not running", urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)

    elisp_cmd = f'(dired "{path}")'
    if os.path.isfile(path):
        elisp_cmd = f'(find-file "{path}")'

    emacs_cmd = f'emacsclient -c -s /run/user/{os.getuid()}/emacs/server -e \'{elisp_cmd}\' &'
    shell_cmd(emacs_cmd, oneshot=True)
