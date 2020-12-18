import argparse
import datetime
import os
import sys

import yaml

from pystdlib.browser import qutebrowser_get_session_entries_org

SESSIONS_PATH = f'{os.environ["HOME"]}/.local/share/qutebrowser/sessions/'
DUMPS_PATH = f'{os.environ["HOME"]}/docs/org/qutebrowser/'
DEFAULT_SESSION = "default.yml"


parser = argparse.ArgumentParser(description="Dump QB session.")
parser.add_argument("--name", "-n", dest="dump_name",
                    help="Session dump filename")
args = parser.parse_args()

dump_name = None
if args.dump_name:
    dump_name = args.dump_name
else:
    dump_name = f"@qutebrowserSessionsNameTemplate@-{datetime.datetime.now().strftime('%d-%m-%Y-%H-%M-%S')}"

session_data_org = None
with open(SESSIONS_PATH + DEFAULT_SESSION, "r") as s:
    session_data_org = qutebrowser_get_session_entries_org(yaml.load(s))

if session_data_org:
    org_lines = []
    count = 1
    for window in session_data_org:
        org_lines.append(f"* window {count}\n")
        count += 1
        for entry in window:
            org_lines.append(f"** {entry}\n")

    with open(DUMPS_PATH + dump_name + ".org", "w") as s:
        s.writelines(org_lines)
