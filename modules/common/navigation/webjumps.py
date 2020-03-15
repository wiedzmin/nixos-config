import json
import os
import subprocess

import dmenu
import redis


r = redis.Redis(host='localhost', port=6379, db=0)

webjumps = json.loads(r.get("nav/webjumps"))

webjump = dmenu.show(webjumps.keys(), prompt="jump to", lines=15)
if webjump:
    vpn_meta = json.loads(r.get("nav/webjumps_vpn"))
    webjump_vpn = vpn_meta.get(webjump, None)
    if webjump_vpn:
        vpn_is_up = r.get("vpn/{0}/is_up".format(webjump_vpn)).decode() == "yes"
        if not vpn_is_up:
            vpn_start_task = subprocess.Popen("vpnctl --start {0}".format(webjump_vpn),
                                              shell=True, stdout=subprocess.PIPE)
            assert vpn_start_task.wait() == 0

    browser_cmd = webjumps[webjump]
    os.system("{0}".format(browser_cmd))
