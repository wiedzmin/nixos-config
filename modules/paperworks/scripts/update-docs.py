import argparse
import json
import sys

import redis

from pystdlib import shell_cmd


r = redis.Redis(host='localhost', port=6379, db=0)

parser = argparse.ArgumentParser(description="update office documents cache")
parser.add_argument("--root", dest="root", help="Document root to process")

args = parser.parse_args()
if not args.root:
    print("No root path provided")
    sys.exit(1)

books = []
books.extend(shell_cmd(f"@docsSearchCommand@ {args.root}", split_output="\n"))

r.set("paperworks/docs", json.dumps(books))