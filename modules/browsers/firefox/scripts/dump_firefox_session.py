import argparse
import datetime
import json
import os
import sys

from lz4.block import decompress

from pystdlib.uishim import notify, URGENCY_CRITICAL


parser = argparse.ArgumentParser(description="Repository overview search")
parser.add_argument('--raw', dest="raw_urls", action="store_true", help="Dump raw URLs")
parser.add_argument('--out', dest="dump_name", type=str, help="Dump filename")
parser.add_argument('--dump-path', dest="dump_path", type=str, help="Path to store dumps under")
parser.add_argument('--dump-basename', dest="dump_basename", default="firefox-session-auto",
                    type=str, help="Dump basename")
parser.add_argument('--sessionstore', dest="sessionstore_path",
                    default=f"{os.getenv('HOME')}/.mozilla/firefox/profile.default/sessionstore-backups",
                    type=str, help="Session store path")
args = parser.parse_args()

if not args.dump_path:
    notify(f"[Firefox]", f"Cannot guess dump path, exiting...", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

session_previous = args.sessionstore_path + "/previous.jsonlz4"
session_recovery = args.sessionstore_path + "/recovery.jsonlz4"

session_dbfile = session_previous
if os.path.exists(session_recovery) and os.path.isfile(session_recovery):
    session_dbfile = session_recovery

session_dumpfile = args.dump_basename + f"-{datetime.datetime.now().strftime('%d-%m-%Y-%H-%M-%S')}.org"
if args.dump_name:
    session_dumpfile = f"{args.dump_name}.org"

result = []
count = 0
with open(session_dbfile, mode='rb') as f:
    b = f.read()
    if b[:8] == b'mozLz40\0':
        b = decompress(b[8:])
    else:
        print("Invalid format")
        sys.exit(1)
    session_data = json.loads(b)
    result = []
    count = 0
    for window in session_data["windows"]:
        for tab in window["tabs"]:
            for entry in tab["entries"]:
                if args.raw_urls:
                    result.append(f"* {entry['url']}\n")
                else:
                    result.append(f"* [[{entry['url']}][{entry['title']}]]\n")
                count += 1

if result and count:
    with open(f"{args.dump_path}/{session_dumpfile}", "w") as f:
        f.writelines(result)

    notify(f"[Firefox]", f"Saved session ({count} tabs)", timeout=5000)
