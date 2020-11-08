import argparse
import json
import os

import redis

from pystdlib.uishim import get_selection
from pystdlib import shell_cmd


parser = argparse.ArgumentParser(description="Webjumps")
parser.add_argument("--fallback", dest="use_fallback", action="store_true",
                    default=False, help="Use fallback browser to open URL")
args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)
webjumps = json.loads(r.get("nav/webjumps"))


webjump = get_selection(webjumps.keys(), "jump to", case_insensitive=True, lines=15, font="@wmFontDmenu@")
if webjump:
    vpn = webjumps[webjump].get("vpn", None)
    if vpn:
        shell_cmd(f"vpnctl --start {vpn}")

    browser_cmd = webjumps[webjump].get("browser", "@defaultBrowser@")
    if args.use_fallback:
        browser_cmd = "@fallbackBrowser@"

    shell_cmd(f"{browser_cmd} {webjumps[webjump]['url']}", oneshot=True)
