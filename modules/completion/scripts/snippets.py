import argparse
import json
import sys

import redis

from pystdlib.uishim import get_selection_rofi
from pystdlib import shell_cmd

parser = argparse.ArgumentParser(description="Snippets")
parser.add_argument('--selector-font', dest="selector_font", type=str, help="Selector font")
args = parser.parse_args()


r = redis.Redis(host='localhost', port=6379, db=0)
snippets = json.loads(r.get("nav/snippets"))

if not len(snippets):
    notify("[snippets]", "No entries", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

snippet = get_selection_rofi(snippets.keys(), "")
if snippet:
    snippet_data = snippets[snippet]
    shell_cmd(["xsel", "-ib"], universal_newlines=True,
              input=f"{snippet_data}")
