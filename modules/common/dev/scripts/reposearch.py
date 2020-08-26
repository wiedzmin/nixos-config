import sys

from pystdlib.uishim import get_selection, notify
from pystdlib.shell import tmux_create_window
from pystdlib import shell_cmd


keyword = shell_cmd("xsel -o -b")
keyword_result = get_selection([keyword] if keyword else [], 'keyword', font="@wmFontDmenu@")
if not keyword_result:
    notify("[search repos]", "no keyword provided", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

matching_repos = shell_cmd(f"fd -t d -d 4 {keyword_result} @searchReposRoot@", split_output="\n")
selected_repo = get_selection(matching_repos, 'repo', case_insensitive=True, lines=10, font="@wmFontDmenu@")
if not selected_repo:
    notify("[search repos]", "no repository selected", timeout=5000)
    sys.exit(0)

tmux_create_window(None, "@tmuxDefaultSession@", window_title=selected_repo.split("/")[-1],
                   start_directory=selected_repo)
elisp_cmd = f'(dired "{selected_repo}")'
emacs_cmd = f'emacsclient -c -s /run/user/1000/emacs/server -e \'{elisp_cmd}\'' # TODO: make SPOT for socket path
shell_cmd(emacs_cmd)
