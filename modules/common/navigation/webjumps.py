import json
import os
import subprocess

import dmenu
import redis


r = redis.Redis(host='localhost', port=6379, db=0)

webjumps = json.loads(r.get("nav/webjumps"))

webjump = dmenu.show(webjumps.keys(), prompt="jump to", case_insensitive=True, lines=15, font="@wmFontDmenu@")
if webjump:
    vpn_meta = json.loads(r.get("nav/webjumps_vpn"))
    webjump_vpn = vpn_meta.get(webjump, None)
    if webjump_vpn:
        vpn_start_task = subprocess.Popen(f"vpnctl --start {webjump_vpn}",
                                          shell=True, stdout=subprocess.PIPE)
        assert vpn_start_task.wait() == 0

    browser_cmd = webjumps[webjump]
    os.system(f"{browser_cmd}")
