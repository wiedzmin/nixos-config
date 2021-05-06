import argparse
import os
import sys

from pystdlib import shell_cmd
from pystdlib.shell import tmux_create_window
from pystdlib.systemd import is_systemd_service_active
from pystdlib.uishim import get_selection_rofi, notify, URGENCY_CRITICAL, log_error


parser = argparse.ArgumentParser(description="Repository overview search")
parser.add_argument('--root', dest="search_root", type=str, help="Root directory to search repositories under")
parser.add_argument('--depth', dest="search_depth", type=int, help="Search depth")
parser.add_argument('--selector-font', dest="selector_font", type=str, help="Selector font")
args = parser.parse_args()

if not args.search_root:
    notify("[search repos]", "no search root provided", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

search_depth = args.search_depth or 4

keyword_result = get_selection_rofi([], 'keyword')
if not keyword_result:
    notify("[search repos]", "no keyword provided", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

matching_repos = shell_cmd(f"fd -t d -d {search_depth} {keyword_result} {args.search_root}", split_output="\n")
selected_repo = get_selection_rofi(matching_repos, 'repo')
if not selected_repo:
    notify("[search repos]", "no repository selected", timeout=5000)
    sys.exit(0)

if not is_systemd_service_active("emacs", user=True):
    notify("[search repos]", "Emacs service not running", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

elisp_cmd = f'(dired "{selected_repo}")'
emacs_cmd = f'emacsclient -c -s /run/user/{os.getuid()}/emacs/server -e \'{elisp_cmd}\''
shell_cmd(emacs_cmd)
