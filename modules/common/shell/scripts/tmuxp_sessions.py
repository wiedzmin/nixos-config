import os

from pystdlib import shell_cmd
from pystdlib.shell import tmuxp_load_session, tmuxp_collect_sessions
from pystdlib.uishim import get_selection

result = get_selection(sorted(tmuxp_collect_sessions()), 'config', lines=10, font="@wmFontDmenu@")

if result:
    shell_cmd(f"tmuxp load -y -d {os.getenv('HOME')}/tmuxp/{result}.yml")
