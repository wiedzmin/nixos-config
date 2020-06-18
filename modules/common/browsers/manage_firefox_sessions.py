import argparse
import glob
import os
import subprocess
import time

import dmenu

@pythonPatchNotify@

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
    session_name = dmenu.show([], prompt="save as",
                              case_insensitive=True, lines=1)
    if session_name:
        subprocess.Popen(
            "dump_firefox_session {0}".format(session_name),
            shell=True, stdout=subprocess.PIPE)
elif args.open_session:
    session_name = dmenu.show(sorted(collect_sessions()), prompt="open",
                              case_insensitive=True, lines=15)
    if session_name:
        urls = None
        with open("@firefoxSessionsPath@/{0}".format(session_name), "r") as session:
            urls = [url.strip()[2:] for url in session.readlines() if url.startswith("* http")]
        if len(urls) <= @firefoxSessionsSizeThreshold@:
            subprocess.Popen(
                "firefox --new-window {0}".format(urls[0]),
                shell=True, stdout=subprocess.PIPE)
            time.sleep(0.5)
            urls_remainder = " --new-tab ".join(urls[1:])
            if len(urls_remainder):
                subprocess.Popen(
                    "firefox --new-tab {0}".format(urls_remainder),
                    shell=True, stdout=subprocess.PIPE)
        else:
            emacsclient_task = subprocess.Popen(
                "emacsclient -c @firefoxSessionsPath@/{0}".format(session_name),
                shell=True, stdout=subprocess.PIPE)
            assert emacsclient_task.wait() == 0
elif args.edit_session:
    session_name = dmenu.show(sorted(collect_sessions()), prompt="edit",
                              case_insensitive=True, lines=15)
    if session_name:
        emacsclient_task = subprocess.Popen(
            "emacsclient -c @firefoxSessionsPath@/{0}".format(session_name),
            shell=True, stdout=subprocess.PIPE)
        assert emacsclient_task.wait() == 0
elif args.delete_session:
    session_name = dmenu.show(sorted(collect_sessions()), prompt="delete",
                              case_insensitive=True, lines=15)
    if session_name:
        subprocess.Popen(
            "rm @firefoxSessionsPath@/{0}".format(session_name),
            shell=True, stdout=subprocess.PIPE)
        notify("[Firefox]", "Removed {0}".format(session_name), timeout=5000)
