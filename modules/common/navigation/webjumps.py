import json
import os

import dmenu
import redis

import redis
r = redis.Redis(host='localhost', port=6379, db=0)

webjumps = json.loads(r.get("nav/webjumps"))
webjumps.update(json.loads(r.get("job/webjumps")))

webjump = dmenu.show(webjumps.keys(), prompt="jump to", lines=15)
if webjump:
    browser_cmd = webjumps[webjump]
    os.system("{0} {1}".format(browser_cmd, webjump))
