import json
import os
import subprocess
import sys

import dmenu
import redis


r = redis.Redis(host='localhost', port=6379, db=0)
dbms_meta = json.loads(r.get("misc/dbms_meta"))

@pythonPatchNotify@

if not len(dbms_meta):
    notify("[dbms]", "No entries", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

dbms_entry = dmenu.show(dbms_meta.keys(), lines=5)
if dbms_entry:
    dbms_pass = None
    if dbms_meta[dbms_entry].get("passwordPassPath"): # using pass
        dbms_pass_task = subprocess.Popen(f'pass {dbms_meta[dbms_entry]["passwordPassPath"]}',
                                          shell=True, stdout=subprocess.PIPE)
        dbms_pass = dbms_pass_task.stdout.read().decode().split("\n")[0]
        assert dbms_pass_task.wait() == 0
    elif dbms_meta[dbms_entry].get("password"): # password in plaintext
        dbms_pass = dbms_meta[dbms_entry].get("password")
    else:
        notify("[dbms]", f"No password provided for '{dbms_entry}'",
               urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)

    dbms_vpn = dbms_meta[dbms_entry].get("vpn", None)
    if dbms_vpn:
        vpn_is_up = r.get(f"vpn/{dbms_vpn}/is_up").decode() == "yes"
        if not vpn_is_up:
            vpn_start_task = subprocess.Popen(f"vpnctl --start {dbms_vpn}",
                                              shell=True, stdout=subprocess.PIPE)
            assert vpn_start_task.wait() == 0

    if dbms_meta[dbms_entry]["command"] == "mycli":
        cmd = f"@mycliBinary@ --host {dbms_meta[dbms_entry]['ip']} --user {dbms_meta[dbms_entry]['user']} --password {dbms_pass}"
        os.system(f'tmux new-window "{cmd}"')
    elif dbms_meta[dbms_entry]["command"] == "pgcli":
        # TODO: elaborate more sophisticated cmd construction logic
        cmd = f"PGPASSWORD={dbms_pass} @pgcliBinary@ --host {dbms_meta[dbms_entry]['ip']} --user {dbms_meta[dbms_entry]['user']} --no-password"
        os.system(f'tmux new-window "{cmd}"')
