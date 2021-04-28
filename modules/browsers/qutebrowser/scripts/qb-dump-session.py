import argparse
import datetime
import os
import sys

from yaml import load, Loader

from pystdlib.browser import qutebrowser_get_session_entries_org
from pystdlib.uishim import notify, URGENCY_CRITICAL


parser = argparse.ArgumentParser(description="Dump QB session.")
parser.add_argument("--name", "-n", dest="dump_name",
                    help="Session dump filename")
parser.add_argument("--flat", dest="flat", action="store_true",
                    help="Flatten session contents (drop windows mapping)")
parser.add_argument('--dump-path', dest="dump_path", type=str, help="Path to store dumps under")
parser.add_argument('--dump-basename', dest="dump_basename", default="qutebrowser-session-auto",
                    type=str, help="Dump basename")
parser.add_argument('--session-file', dest="session_file", default="default.yml",
                    type=str, help="Session filename")
parser.add_argument('--sessionstore', dest="sessionstore_path",
                    default=f"{os.environ['HOME']}/.local/share/qutebrowser/sessions",
                    type=str, help="Session store path")
args = parser.parse_args()

if not args.dump_path:
    notify(f"[Qutebrowser]", f"Cannot guess dump path, exiting...", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

dump_name = None
if args.dump_name:
    dump_name = args.dump_name
else:
    dump_name = f"{args.dump_basename}-{datetime.datetime.now().strftime('%d-%m-%Y-%H-%M-%S')}"

session_data_org = None
with open(f"{args.sessionstore_path}/{args.session_file}", "r") as s:
    session_data_org = qutebrowser_get_session_entries_org(load(s, Loader=Loader))

if session_data_org:
    org_lines = []
    count = 1
    for window in session_data_org:
        if not args.flat:
            org_lines.append(f"* window {count}\n")
            count += 1
        for entry in window:
            if args.flat:
                org_lines.append(f"* {entry}\n")
            else:
                org_lines.append(f"** {entry}\n")

    with open(f"{args.dump_path}/{dump_name}.org", "w") as s:
        s.writelines(org_lines)
