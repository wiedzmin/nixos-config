import argparse
import json
import os
import re

import redis

from pystdlib.uishim import get_selection_rofi
from pystdlib import shell_cmd


parser = argparse.ArgumentParser(description="Searchengines")
parser.add_argument('--selector-font', dest="selector_font", type=str, help="Selector font")
args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)

documents = r.get("paperworks/docs")
if documents:
    documents = json.loads(documents)

result = get_selection_rofi(documents, 'document')
if result:
    shell_cmd(f"libreoffice {re.escape(result)}")
