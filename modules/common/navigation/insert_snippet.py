import json
import os

import dmenu
import redis

import redis
r = redis.Redis(host='localhost', port=6379, db=0)

snippets = json.loads(r.get("misc/snippets"))

snippet = dmenu.show(snippets, prompt="insert", case_insensitive=True, lines=10)
if snippet:
    os.system("setxkbmap && xdotool type \"{0}\"".format(snippet))
