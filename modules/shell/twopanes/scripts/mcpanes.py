import argparse
import json
import redis

from pystdlib import shell_cmd
from pystdlib.shell import tmux_create_window
from pystdlib.uishim import get_selection_rofi
from pystdlib.xlib import switch_named_desktop


parser = argparse.ArgumentParser(description="Two panes file manager selection")
parser.add_argument('--selector-font', dest="selector_font", type=str, help="Selector font")
args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)
bookmarks = json.loads(r.get("nav/bookmarks"))

left_pane_path = get_selection_rofi(bookmarks.keys(), "left") or " "
right_pane_path = get_selection_rofi(bookmarks.keys(), "right") or " "

cmd = f"mc {left_pane_path} {right_pane_path}".strip(" ")

tmux_create_window(cmd, "main", window_title="copier", create_if_not=True, attach=True)
# shell_cmd(cmd, oneshot=True)
