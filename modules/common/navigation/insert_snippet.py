import json
import os

import dmenu
import redis

import redis
r = redis.Redis(host='localhost', port=6379, db=0)

snippets = json.loads(r.get("misc/snippets"))

snippet = dmenu.show(snippets, prompt="insert", lines=10)
if snippet:
    os.system("@xdotoolBinary@ type \"{0}\"".format(snippet))
