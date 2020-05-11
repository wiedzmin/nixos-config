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

result = dmenu.show(ebooks, prompt='book', case_insensitive=True, lines=30)
if result:
    subprocess.Popen("zathura {0}".format(re.escape(result)), shell=True, stdout=subprocess.PIPE)
