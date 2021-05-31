import argparse
import json
import os
import re

import redis

from pystdlib.uishim import get_selection_rofi
from pystdlib import shell_cmd


parser = argparse.ArgumentParser(description="Searchengines")
parser.add_argument('--selector-font', dest="selector_font", type=str, help="Selector font")
args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)

ebooks = []
for key in r.scan_iter("content/*/ebooks"):
    value = r.get(key)
    if value:
        ebooks.extend(json.loads(value))

result = get_selection_rofi(ebooks, 'book')
if result:
    shell_cmd(f"zathura {re.escape(result)}")
