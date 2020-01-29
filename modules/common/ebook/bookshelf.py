import os
import re
import subprocess

import dmenu

books = []

books_task = subprocess.Popen("@fdBinary@ --full-path @bookshelfPath@ -e pdf -e djvu -e epub",
                              shell=True, stdout=subprocess.PIPE)
books.extend([book for book in books_task.stdout.read().decode().split("\n")])
assert books_task.wait() == 0

result = dmenu.show(books, prompt='book', lines=30)
if result:
    os.system("@bookReader@ {0}".format(re.escape(result)))
