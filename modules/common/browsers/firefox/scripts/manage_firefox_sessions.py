import argparse
import glob
import os
import time

from pystdlib.uishim import get_selection, notify
from pystdlib import shell_cmd


def collect_sessions():
    return [os.path.basename(session) for session in glob.glob("@firefoxSessionsPath@/*.org")]


parser = argparse.ArgumentParser(description="Manage Firefox stored sessions.")
parser.add_argument("--save", dest="save_session", action="store_true",
                   default=False, help="Save current session")
parser.add_argument("--open", dest="open_session", action="store_true",
                   default=False, help="Open stored session")
parser.add_argument("--edit", dest="edit_session", action="store_true",
                   default=False, help="Edit stored session")
parser.add_argument("--delete", dest="delete_session", action="store_true",
                   default=False, help="Delete stored session")

args = parser.parse_args()

if args.save_session:
    session_name = get_selection([], "save as", case_insensitive=True, lines=1, font="@wmFontDmenu@")
    if session_name:
        shell_cmd(f"dump_firefox_session {session_name}")
elif args.open_session:
    session_name = get_selection(sorted(collect_sessions()), "open", case_insensitive=True, lines=15, font="@wmFontDmenu@")
    if session_name:
        urls = None
        with open(f"@firefoxSessionsPath@/{session_name}", "r") as session:
            urls = [url.strip()[2:] for url in session.readlines() if url.startswith("* http")]
        if len(urls) <= @firefoxSessionsSizeThreshold@:
            shell_cmd(f"firefox --new-window {urls[0]}")
            time.sleep(0.5)
            urls_remainder = " --new-tab ".join(urls[1:])
            if len(urls_remainder):
                shell_cmd(f"firefox --new-tab {urls_remainder}")
        else:
            shell_cmd(f"emacsclient -c @firefoxSessionsPath@/{session_name}")
elif args.edit_session:
    session_name = get_selection(sorted(collect_sessions()), "edit", case_insensitive=True, lines=15, font="@wmFontDmenu@")
    if session_name:
        shell_cmd(f"emacsclient -c @firefoxSessionsPath@/{session_name}")
elif args.delete_session:
    session_name = get_selection(sorted(collect_sessions()), "delete", case_insensitive=True, lines=15, font="@wmFontDmenu@")
    if session_name:
        shell_cmd(f"rm @firefoxSessionsPath@/{session_name}")
        notify("[Firefox]", f"Removed {session_name}", timeout=5000)
