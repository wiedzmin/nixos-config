import json
import sys

import redis

from pystdlib.uishim import get_selection, notify, URGENCY_CRITICAL
from pystdlib.shell import tmux_create_window
from pystdlib import shell_cmd


r = redis.Redis(host='localhost', port=6379, db=0)
dbms_meta = json.loads(r.get("misc/dbms_meta"))
extra_hosts_data = json.loads(r.get("net/extra_hosts")) # TODO: deduplicate other metas further


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

    dbms_vpn = dbms_meta[dbms_entry].get("vpn", None)
    if dbms_vpn:
        shell_cmd(f"vpnctl --start {dbms_vpn}")

    host = dbms_meta[dbms_entry]['host']

    if dbms_meta[dbms_entry]["command"] == "mycli":
        cmd = f"@mycliCmd@ --host {host} --user {dbms_meta[dbms_entry]['user']} --password {dbms_pass}"
    elif dbms_meta[dbms_entry]["command"] == "pgcli":
        # TODO: elaborate more sophisticated cmd construction logic
        cmd = f"PGPASSWORD={dbms_pass} @pgcliCmd@ --host {dbms_meta[dbms_entry]['host']} --user {dbms_meta[dbms_entry]['user']} --no-password"

    tmux_create_window(cmd, extra_hosts_data[host].get("tmux", "@tmuxDefaultSession@"),
                       window_title=dbms_entry, create_if_not=True, attach=True)
