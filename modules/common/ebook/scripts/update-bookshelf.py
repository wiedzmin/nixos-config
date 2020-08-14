import subprocess
import json

import redis

r = redis.Redis(host='localhost', port=6379, db=0)

roots = json.loads(r.get("content/ebook_roots"))

books = []

for root in roots:
    books_task = subprocess.Popen(f"@booksSearchCommand@ {root}", shell=True, stdout=subprocess.PIPE)
    books.extend([book for book in books_task.stdout.read().decode().strip().split("\n")])

r.set("content/ebooks_list", json.dumps(books))
