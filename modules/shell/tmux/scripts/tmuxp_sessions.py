import argparse
import os

from pystdlib import shell_cmd
from pystdlib.shell import tmuxp_load_session, tmuxp_collect_sessions
from pystdlib.uishim import get_selection

parser = argparse.ArgumentParser(description="Tmuxp session selection")
parser.add_argument('--dmenu-font', dest="dmenu_font", type=str, help="Dmenu font")
args = parser.parse_args()


result = get_selection(sorted(tmuxp_collect_sessions()), 'config', lines=10,
                       font=args.dmenu_font)

if result:
    shell_cmd(f"tmuxp load -y -d {os.getenv('HOME')}/.tmuxp/{result}.yml")
