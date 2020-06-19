import json
import os
import subprocess

import dmenu
import redis

import redis
r = redis.Redis(host='localhost', port=6379, db=0)

searchengines = json.loads(r.get("nav/searchengines"))

searchengine = dmenu.show(searchengines.keys(), prompt="search with",
                          case_insensitive=True, lines=15)
if searchengine:
    searchengine_url = searchengines[searchengine]
    search_term = dmenu.show([], prompt="term")
    if search_term:
        subprocess.run(f'@defaultBrowser@ {searchengine_url}{search_term.replace(" ", "+")}'.split())
