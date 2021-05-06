import argparse
import os

from pystdlib import shell_cmd
from pystdlib.shell import tmuxp_load_session, tmuxp_collect_sessions
from pystdlib.uishim import get_selection_rofi

parser = argparse.ArgumentParser(description="Tmuxp session selection")
parser.add_argument('--selector-font', dest="selector_font", type=str, help="Selector font")
args = parser.parse_args()


result = get_selection_rofi(sorted(tmuxp_collect_sessions()), 'session')

if result:
    shell_cmd(f"tmuxp load -y -d {os.getenv('HOME')}/.tmuxp/{result}.yml")
