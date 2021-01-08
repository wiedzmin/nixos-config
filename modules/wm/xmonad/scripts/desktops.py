import argparse
import heapq
import itertools
import json
import sys

import redis
from fuzzywuzzy import fuzz # TODO: play with https://pypi.org/project/pyjarowinkler/
from ewmh import EWMH

from pystdlib import shell_cmd
from pystdlib.xlib import prepare_desktops_map


SIMILARITY_GROUP_TRESHOLD = 1
SIMILARITY_DIRECT_TRESHOLD = 20


parser = argparse.ArgumentParser(description="Org-capture proxy.")
parser.add_argument("--init", "-i", dest="init", action="store_true",
                    help="Init desktops metadata")
parser.add_argument("--verbose", "-v", dest="verbosity", action="count", default=0,
                    help="Manage verbosity")
parser.add_argument("--grep", "-g", dest="needle",
                    help="Manage verbosity")
parser.add_argument("--dry-run", "-0", dest="dry_run", action="store_true",
                    help="Do not change anything")
args = parser.parse_args()
r = redis.Redis(host='localhost', port=6379, db=0)

if args.init:
    desktops_map = prepare_desktops_map()
    if not desktops_map:
        sys.exit(128)
    r.set("xserver/desktops_map", json.dumps(desktops_map))
    sys.exit(0)

desktops_map = json.loads(r.get("xserver/desktops_map"))
window_rules = json.loads(r.get("xserver/window_rules"))

ewmh = EWMH()
NET_WM_NAME = ewmh.display.intern_atom('_NET_WM_NAME')
UTF8_STRING = ewmh.display.intern_atom('UTF8_STRING')
windows = ewmh.getClientList()

for window in sorted(windows, key=lambda w: w.get_full_text_property(NET_WM_NAME, UTF8_STRING)):
    similarities = {}
    rule = None
    desktop_name = None
    print("================================")
    window_title = window.get_full_text_property(NET_WM_NAME, UTF8_STRING)
    window_class = window.get_wm_class()[1]
    if args.verbosity >= 1:
        print(f"window ({window_class}): {window_title}")
    for tokens in window_rules.keys():
        ratio = fuzz.token_set_ratio(" ".join(tokens.split()[1:]), window_title, force_ascii=False)
        similarities.setdefault(ratio, []).append(tokens)
    sim_groups = {k: list(g) for k, g in itertools.groupby(sorted(similarities.keys()), key=lambda n: n // 10)}
    group_max, group_second = heapq.nlargest(2, sim_groups.keys())
    direct_max, direct_second = heapq.nlargest(2, similarities.keys())
    group_delta = group_max - group_second
    direct_delta = direct_max - direct_second
    if args.verbosity >= 3:
        for k, g in sim_groups.items():
            print(f"{k}: {g}")
        print("--------------------------------")
        for sim, rule in sorted(similarities.items()):
            print(f"{sim}: {rule}")
    rule = similarities[sim_groups[group_max][0]][0]
    rule_class = rule.split()[0]
    desktop_name = window_rules[rule]
    desktop_index = int(desktops_map[desktop_name])
    move_window = False
    if window_class != rule_class:
        if args.verbosity >= 1:
            print(f"skipping window, overriding rule, non-matching window classes: {window_class} != {rule_class}")
    else:
        if args.verbosity >= 1:
            print(f"window classes matched: {window_class} == {rule_class}, checking rule...")
        if group_delta > SIMILARITY_GROUP_TRESHOLD or direct_delta < SIMILARITY_DIRECT_TRESHOLD:
            move_window = True
            if args.verbosity >= 1:
                print(f"rule: '{rule}' fired")
        elif window_class == rule_class:
            move_window = True
            if args.verbosity >= 1:
                print("no rule fired, but window classes matched, moving window")

    if move_window:
        if args.verbosity >= 1:
            print(f"--> {desktop_name}")
        if not args.dry_run:
            ewmh.setWmDesktop(window, desktop_index)

ewmh.display.flush()
