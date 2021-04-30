import argparse
import json
import os
import re

import redis

from pystdlib.uishim import get_selection
from pystdlib import shell_cmd


parser = argparse.ArgumentParser(description="Searchengines")
parser.add_argument('--dmenu-font', dest="dmenu_font", type=str, help="Dmenu font")
args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)

ebooks = r.get("content/ebooks_list")
if ebooks:
    ebooks = json.loads(ebooks)

result = get_selection(ebooks, 'book', case_insensitive=True, lines=30, font=args.dmenu_font)
if result:
    shell_cmd(f"zathura {re.escape(result)}")
