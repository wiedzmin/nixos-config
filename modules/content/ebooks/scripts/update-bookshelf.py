import argparse
import json
import sys

import redis

from pystdlib import shell_cmd


r = redis.Redis(host='localhost', port=6379, db=0)

parser = argparse.ArgumentParser(description="update ebooks root contents")
parser.add_argument("--root", dest="root", help="Ebooks root to process")
parser.add_argument("--search-command", dest="search_command", help="Ebooks search command")
args = parser.parse_args()

if not args.root:
    print("No root path provided")
    sys.exit(1)

books = []
books.extend(shell_cmd(f"{args.search_command} {args.root}", split_output="\n"))

r.set("content/ebooks_list", json.dumps(books))
