import sys

from pystdlib import shell_cmd
from pystdlib.shell import tmux_create_window
from pystdlib.systemd import is_systemd_service_active
from pystdlib.uishim import get_selection, notify, URGENCY_CRITICAL, log_error


keyword_result = get_selection([], 'keyword', font="@wmFontDmenu@")
if not keyword_result:
    notify("[search repos]", "no keyword provided", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

matching_repos = shell_cmd(f"fd -t d -d @searchDepth@ {keyword_result} @searchReposRoot@", split_output="\n")
selected_repo = get_selection(matching_repos, 'repo', case_insensitive=True, lines=10, font="@wmFontDmenu@")
if not selected_repo:
    notify("[search repos]", "no repository selected", timeout=5000)
    sys.exit(0)

if not is_systemd_service_active("emacs", user=True):
    notify("[search repos]", "Emacs service not running", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

elisp_cmd = f'(dired "{selected_repo}")'
emacs_cmd = f'emacsclient -c -s /run/user/1000/emacs/server -e \'{elisp_cmd}\'' # TODO: make SPOT for socket path
shell_cmd(emacs_cmd)
