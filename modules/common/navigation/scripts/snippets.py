import json
import sys

import redis

from pystdlib.uishim import get_selection
from pystdlib import shell_cmd


r = redis.Redis(host='localhost', port=6379, db=0)
snippets = json.loads(r.get("nav/snippets"))

if not len(snippets):
    notify("[snippets]", "No entries", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

snippet = get_selection(snippets.keys(), "", lines=15, font="@wmFontDmenu@")
if snippet:
    snippet_data = snippets[snippet]
    shell_cmd(["xsel", "-ib"], universal_newlines=True,
              input=f"{snippet_data}")
