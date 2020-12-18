import json

import redis

from pystdlib import shell_cmd


r = redis.Redis(host='localhost', port=6379, db=0)
roots = json.loads(r.get("content/ebook_roots"))

books = []
for root in roots:
    books.extend(shell_cmd(f"@booksSearchCommand@ {root}", split_output="\n"))

r.set("content/ebooks_list", json.dumps(books))
