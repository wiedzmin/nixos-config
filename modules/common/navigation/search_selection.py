import json
import os
import subprocess

import dmenu
import redis

import redis
r = redis.Redis(host='localhost', port=6379, db=0)

searchengines = json.loads(r.get("nav/searchengines"))

searchengine = dmenu.show(searchengines.keys(), prompt="search with", case_insensitive=True, lines=15)
if searchengine:
    searchengine_url = searchengines[searchengine]
    search_term_task = subprocess.Popen("xsel -o", shell=True, stdout=subprocess.PIPE)
    search_term = search_term_task.stdout.read().decode()
    assert search_term_task.wait() == 0
    subprocess.run(f'@defaultBrowser@ {searchengine_url}{search_term.replace(" ", "+")}'.split())
