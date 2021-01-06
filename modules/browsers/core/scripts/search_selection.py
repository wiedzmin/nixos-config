import argparse
import json
import os

import redis

from pystdlib.uishim import get_selection
from pystdlib import shell_cmd


parser = argparse.ArgumentParser(description="Searchengines")
parser.add_argument("--fallback", dest="use_fallback", action="store_true",
                    default=False, help="Use fallback browser to open URL")
args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)
searchengines = json.loads(r.get("nav/searchengines"))


searchengine = get_selection(searchengines.keys(), "search with", case_insensitive=True, lines=15, font="@wmFontDmenu@")
if searchengine:
    meta = searchengines[searchengine]
    url = meta["url"]

    browser_cmd = meta.get("browser", "@defaultBrowser@")
    if args.use_fallback:
        browser_cmd = "@fallbackBrowser@"

    vpn = meta.get("vpn", None)
    if vpn:
        shell_cmd(f"vpnctl --start {vpn}")

    search_term = shell_cmd("xsel -o").replace(" ", "+")
    shell_cmd(f'{browser_cmd} {url}{search_term}'.split(), shell=False)
