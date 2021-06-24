import argparse
import json
import sys

import redis

from pystdlib.uishim import get_selection_rofi, notify, URGENCY_CRITICAL
from pystdlib.shell import term_create_window, tmux_create_window
from pystdlib import shell_cmd


parser = argparse.ArgumentParser(description="DBMS connectivity")
parser.add_argument('--tmux-session', dest="tmux_session", default="main", type=str, help="Fallback tmux session name")
parser.add_argument("--ignore-tmux", dest="ignore_tmux", action="store_true",
                   default=False, help="open connection in new terminal window rather than tmux pane")
parser.add_argument('--term-command', dest="term_command", type=str, help="Terminal command")
parser.add_argument('--selector-font', dest="selector_font", type=str, help="Selector font")
parser.add_argument('--secret-ph', dest="secret_placeholder", default="0xDEADF00D", type=str, help="Secret placeholder token")
args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)
dbms_meta = json.loads(r.get("misc/dbms_meta"))
extra_hosts_data = json.loads(r.get("net/extra_hosts"))


if not len(dbms_meta):
    notify("[dbms]", "No entries", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)
if not args.term_command:
    notify("[dbms]", "No terminal command provided", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

dbms_entry = get_selection_rofi(dbms_meta.keys(), "")
if dbms_entry:
    dbms_pass = None

    dbms_entry_meta = dbms_meta[dbms_entry]
    host_meta = extra_hosts_data[dbms_entry_meta["host"]]
    dbms_vpn = host_meta.get("vpn", None)
    if dbms_vpn:
        shell_cmd(f"vpn --start {dbms_vpn}")

    if dbms_entry_meta.get("passwordPassPath"): # using pass
        dbms_pass = shell_cmd(f'pass {dbms_entry_meta["passwordPassPath"]}', split_output="\n")[0]
    elif dbms_entry_meta.get("password"): # password in plaintext
        dbms_pass = dbms_entry_meta.get("password")
    else:
        notify("[dbms]", f"No password provided for '{dbms_entry}'",
               urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)
    cmd = dbms_entry_meta["command"].replace(args.secret_placeholder, dbms_pass)

    if args.ignore_tmux:
        term_create_window(cmd, term_cmd=args.term_command)
    else:
        target_session = host_meta.get("tmux", args.tmux_session)
        result = tmux_create_window(cmd, session_name=target_session, window_title=f"dbms :: {dbms_entry}",
                                    attach=False)
        if not result:
            notify("[dbms]", "error creating tmux window", urgency=URGENCY_CRITICAL)
            sys.exit(1)
        else:
            result = term_create_window(f"tmux attach -t {target_session}", term_cmd=args.term_command)
