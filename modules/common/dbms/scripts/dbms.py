import json
import sys

import redis

from pystdlib.uishim import get_selection, notify, URGENCY_CRITICAL
from pystdlib.shell import tmux_create_window
from pystdlib import shell_cmd


r = redis.Redis(host='localhost', port=6379, db=0)
dbms_meta = json.loads(r.get("misc/dbms_meta"))
extra_hosts_data = json.loads(r.get("net/extra_hosts"))


if not len(dbms_meta):
    notify("[dbms]", "No entries", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

dbms_entry = get_selection(dbms_meta.keys(), "", lines=5, font="@wmFontDmenu@")
if dbms_entry:
    dbms_pass = None
    if dbms_meta[dbms_entry].get("passwordPassPath"): # using pass
        dbms_pass = shell_cmd(f'pass {dbms_meta[dbms_entry]["passwordPassPath"]}', split_output="\n")[0]
    elif dbms_meta[dbms_entry].get("password"): # password in plaintext
        dbms_pass = dbms_meta[dbms_entry].get("password")
    else:
        notify("[dbms]", f"No password provided for '{dbms_entry}'",
               urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)

    host = dbms_meta[dbms_entry]["host"]
    dbms_vpn = extra_hosts_data[host].get("vpn", None)
    if dbms_vpn:
        shell_cmd(f"vpnctl --start {dbms_vpn}")

    cmd = dbms_meta[dbms_entry]["command"].replace("@passwordPlaceholder@", dbms_pass)
    tmux_session_name = extra_hosts_data.get(host, dict()).get("tmux", "@tmuxDefaultSession@")
    tmux_create_window(cmd, tmux_session_name, window_title=dbms_entry, create_if_not=True, attach=True)
