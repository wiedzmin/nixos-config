import json
import redis

r = redis.Redis(host='localhost', port=6379, db=0)
bookmarks = json.loads(r.get("nav/bookmarks"))

from pystdlib import shell_cmd
from pystdlib.shell import tmux_create_window
from pystdlib.uishim import get_selection
from pystdlib.xlib import switch_named_desktop

left_pane_path = get_selection(bookmarks.keys(), "left >", lines=15, font="@wmFontDmenu@") or " "
right_pane_path = get_selection(bookmarks.keys(), "right >", lines=15, font="@wmFontDmenu@") or " "

cmd = f"@mcCmd@ {left_pane_path} {right_pane_path}".strip(" ")

tmux_create_window(cmd, "main", window_title="copier", create_if_not=True, attach=True)
# shell_cmd(cmd, oneshot=True)
