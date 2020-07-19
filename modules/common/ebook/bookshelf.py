import json
import os
import re
import subprocess

import dmenu
import redis


r = redis.Redis(host='localhost', port=6379, db=0)

ebooks = r.get("content/ebooks_list")
if ebooks:
    ebooks = json.loads(ebooks)

result = dmenu.show(ebooks, prompt='book', case_insensitive=True, lines=30, font="@wmFontDmenu@")
if result:
    subprocess.Popen(f"zathura {re.escape(result)}", shell=True, stdout=subprocess.PIPE)
