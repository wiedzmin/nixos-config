import argparse
import json
import subprocess
import sys

import redis

from fuzzywuzzy import fuzz
from ewmh import EWMH

def prepare_desktops_map():
    get_desktops_meta_task = subprocess.Popen("wmctrl -d", shell=True,
                                              stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    desktops = get_desktops_meta_task.stdout.read().decode().strip().split("\n")
    result = get_desktops_meta_task.wait()
    if result != 0:
        raise Exception(message=get_desktops_meta_task.stderr.read().decode())
    return { desktop[-1]: desktop[0] for desktop in [desktop.split() for desktop in desktops]}


parser = argparse.ArgumentParser(description="Org-capture proxy.")
parser.add_argument("--init", "-i", dest="init", action="store_true",
                    help="Init desktops metadata")
parser.add_argument("--verbose", "-v", dest="verbosity", action="count", default=0,
                    help="Manage verbosity")
parser.add_argument("--grep", "-g", dest="needle",
                    help="Manage verbosity")
args = parser.parse_args()

r = redis.Redis(host='localhost', port=6379, db=0)
desktops_map = json.loads(r.get("xserver/desktops_map"))
window_rules = json.loads(r.get("xserver/window_rules"))

if args.init:
    r.set("xserver/desktops_map", json.dumps(prepare_desktops_map()))
    sys.exit(0)

ewmh = EWMH()
NET_WM_NAME = ewmh.display.intern_atom('_NET_WM_NAME')
UTF8_STRING = ewmh.display.intern_atom('UTF8_STRING')
windows = ewmh.getClientList()

for window in windows:
    similarities = {}
    window_title = window.get_full_text_property(NET_WM_NAME, UTF8_STRING)
    for tokens in window_rules.keys():
        ratio = fuzz.token_set_ratio(tokens, window_title, force_ascii=False)
        similarities[ratio] = tokens
    rule = None
    desktop_name = None
    max_ = max(similarities.keys())
    if max_ >= 75:
        rule = similarities[max_]
        desktop_name = window_rules[rule]
        if args.verbosity >= 1:
            if args.needle and args.needle not in window_title:
                continue
        print("rule '{0}' fired for '{1}' --> {2}".format(rule, window_title, desktop_name))
    else:
        if args.verbosity >= 2:
            if args.needle and args.needle not in window_title:
                continue
        print("no rule fired for '{0}'".format(window_title))
    if args.verbosity >= 3:
        if args.needle and args.needle not in window_title:
            continue
        print("similarities: {0}\n-----------------".format(similarities))
    if rule:
        desktop_index = int(desktops_map[desktop_name])
        ewmh.setWmDesktop(window, desktop_index)

ewmh.display.flush()
