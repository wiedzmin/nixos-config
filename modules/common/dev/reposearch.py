import subprocess
import sys

from libtmux import Server
from notify2 import URGENCY_NORMAL, URGENCY_CRITICAL
import dmenu
import notify2


notify2.init("reposearch")

keyword_task = subprocess.Popen("xsel -o -b", shell=True, stdout=subprocess.PIPE)
keyword_text = keyword_task.stdout.read().decode().strip()

keyword_result = None
if keyword_text:
    keyword_result = dmenu.show([keyword_text], prompt='keyword')
else:
    keyword_result = dmenu.show([], prompt='keyword')

if not keyword_result:
    n = notify2.Notification("[search repos]", "no keyword provided")
    n.set_urgency(URGENCY_CRITICAL)
    n.set_timeout(5000)
    n.show()
    sys.exit(1)

search_repos_task = subprocess.Popen("fd -t d -d 4 {0} @searchReposRoot@".format(keyword_result),
                                     shell=True, stdout=subprocess.PIPE)
search_repos_result = search_repos_task.stdout.read().decode().strip().split("\n")

selected_repo = dmenu.show(search_repos_result, prompt='repo', case_insensitive=True, lines=10)
if not selected_repo:
    n = notify2.Notification("[search repos]", "no repository selected")
    n.set_urgency(URGENCY_NORMAL)
    n.set_timeout(5000)
    n.show()
    sys.exit(0)

tmux_server = Server()
tmux_session = tmux_server.find_where({ "session_name": "@tmuxDefaultSession@" })
repo_window = tmux_session.new_window(attach=True, window_name=selected_repo.split("/")[-1],
                                      start_directory=selected_repo)

open_emacs_frame_task = subprocess.Popen("emacsclient -c -a '' {0}".format(selected_repo),
                                         shell=True, stdout=subprocess.PIPE)
