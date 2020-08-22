import json
import os

import redis

from pystdlib.uishim import get_selection
from pystdlib import shell_cmd


r = redis.Redis(host='localhost', port=6379, db=0)
webjumps = json.loads(r.get("nav/webjumps"))

webjump = get_selection(webjumps.keys(), "jump to", case_insensitive=True, lines=15, font="@wmFontDmenu@")
if webjump:
    vpn_meta = json.loads(r.get("nav/webjumps_vpn"))
    webjump_vpn = vpn_meta.get(webjump, None)
    if webjump_vpn:
        shell_cmd(f"vpnctl --start {webjump_vpn}")

    browser_cmd = webjumps[webjump]
    shell_cmd(f"{browser_cmd}", oneshot=True)
