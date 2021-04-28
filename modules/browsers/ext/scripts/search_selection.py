import argparse
import json
import os

import redis

from pystdlib.uishim import get_selection
from pystdlib import shell_cmd


parser = argparse.ArgumentParser(description="Searchengines")
parser.add_argument('--browser', dest="default_browser", type=str, help="Default browser")
parser.add_argument('--fallback-browser', dest="fallback_browser", type=str, help="Fallback browser")
parser.add_argument("--use-fallback", dest="use_fallback", action="store_true",
                    default=False, help="Use fallback browser to open URL")
parser.add_argument('--dmenu-font', dest="dmenu_font", type=str, help="Dmenu font")
args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)
searchengines = json.loads(r.get("nav/searchengines"))

if not (args.default_browser and args.fallback_browser):
    notify(f"[Search selection]", f"Browsers not set, exiting...", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

searchengine = get_selection(searchengines.keys(), "search with", case_insensitive=True, lines=15, font=args.dmenu_font)
if searchengine:
    meta = searchengines[searchengine]
    url = meta["url"]

    browser_cmd = meta.get("browser", args.default_browser)
    if args.use_fallback:
        browser_cmd = args.fallback_browser

    vpn = meta.get("vpn", None)
    if vpn:
        shell_cmd(f"vpnctl --start {vpn}")

    search_term = shell_cmd("xsel -o").replace(" ", "+")
    shell_cmd(f'{browser_cmd} {url}{search_term}'.split(), shell=False)
