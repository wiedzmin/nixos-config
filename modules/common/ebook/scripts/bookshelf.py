import json
import os
import re

import redis

from pystdlib.uishim import get_selection
from pystdlib import shell_cmd


r = redis.Redis(host='localhost', port=6379, db=0)

ebooks = r.get("content/ebooks_list")
if ebooks:
    ebooks = json.loads(ebooks)

result = get_selection(ebooks, 'book', case_insensitive=True, lines=30, font="@wmFontDmenu@")
if result:
    shell_cmd(f"zathura {re.escape(result)}")
