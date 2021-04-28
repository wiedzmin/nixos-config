import argparse
import json
import os

import redis

from pystdlib.uishim import get_selection
from pystdlib import shell_cmd


parser = argparse.ArgumentParser(description="Webjumps")
parser.add_argument('--browser', dest="default_browser", type=str, help="Default browser")
parser.add_argument('--fallback-browser', dest="fallback_browser", type=str, help="Fallback browser")
parser.add_argument("--use-fallback", dest="use_fallback", action="store_true",
                    default=False, help="Use fallback browser to open URL")
parser.add_argument('--dmenu-font', dest="dmenu_font", type=str, help="Dmenu font")
parser.add_argument("--copy", dest="copy_url", action="store_true",
                    default=False, help="Copy webjump's url to clipboard")
args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)
webjumps = json.loads(r.get("nav/webjumps"))

if not (args.default_browser and args.fallback_browser) and not args.copy_url:
    notify(f"[Search selection]", f"Browsers not set, exiting...", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

webjump = get_selection(webjumps.keys(), "jump to", case_insensitive=True, lines=15, font=args.dmenu_font)
if webjump:
    vpn = webjumps[webjump].get("vpn", None)
    if vpn:
        shell_cmd(f"vpnctl --start {vpn}")

    url = webjumps[webjump]['url']
    if args.copy_url:
        shell_cmd(["xsel", "-ib"], input=url.encode('utf-8'))
    else:
        browser_cmd = webjumps[webjump].get("browser", args.default_browser)
        if args.use_fallback:
            browser_cmd = args.fallback_browser
        shell_cmd(f"{browser_cmd} {url}", oneshot=True)
