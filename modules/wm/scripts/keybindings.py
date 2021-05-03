import argparse
import json
import os

import redis

from pystdlib.uishim import get_selection, show_text_dialog


parser = argparse.ArgumentParser(description="WM keybindings reference")
parser.add_argument("--fuzzy", dest="fuzzy", action="store_true",
                   default=False, help="Use fuzzy selector, like dmenu, over keybindings reference")
parser.add_argument('--dmenu-font', dest="dmenu_font", type=str, help="Dmenu font")
args = parser.parse_args()


r = redis.Redis(host='localhost', port=6379, db=0)
legend = []


def format_modebinding(name, binding=None, fuzzy=False):
    mode_name = name.split("-")[0].strip()
    mode_binding = binding
    if not mode_binding and not fuzzy:
        mode_binding = "-"
    if type(mode_binding) == list:
        mode_binding = "+".join(binding)
    if fuzzy:
        return f"{mode_binding} " if mode_binding else ""
    return f'MODE: {mode_name} ({mode_binding}):'


def format_keybinding(kb, fuzzy=False, dangling=False):
    cmd_clean = kb["cmd"]
    if "/nix/store" in cmd_clean:
        cmd_clean = cmd_clean.split("/bin/")[-1]
    if fuzzy:
        return f'{"[DANGLING]" if dangling else ""}[{kb["mode"]}] [{format_modebinding(kb["mode"], kb["modebinding"], fuzzy=True)}{"+".join(kb["key"])}]: {cmd_clean}'
    return f'    [{"+".join(kb["key"])}]: {cmd_clean}'


keybindings = json.loads(r.get("wm/keybindings"))
modebindings = json.loads(r.get("wm/modebindings"))

keybindings_by_mode = {}
dangling_keybindings_by_mode = {}

for kb in keybindings:
    if kb["mode"] == "root" or kb["mode"] in modebindings:
        kb["modebinding"] = modebindings.get(kb["mode"], "")
        keybindings_by_mode.setdefault(kb["mode"], []).append(kb)
    else:
        kb["modebinding"] = ""
        dangling_keybindings_by_mode.setdefault(kb["mode"], []).append(kb)

for mode_name in keybindings_by_mode:
    if not args.fuzzy:
        legend.append(format_modebinding(mode_name,
                                         binding=modebindings[mode_name]
                                         if mode_name != "root" else "non-prefix"))
    for kb in keybindings_by_mode[mode_name]:
        legend.append(format_keybinding(kb, fuzzy=args.fuzzy))

if len(dangling_keybindings_by_mode):
    if not args.fuzzy:
        legend.append("================== DANGLING KEYBINDINGS ==================")
    for mode_name in dangling_keybindings_by_mode:
        if not args.fuzzy:
            legend.append(format_modebinding(mode_name))
        for kb in dangling_keybindings_by_mode[mode_name]:
            legend.append(format_keybinding(kb, fuzzy=args.fuzzy, dangling=True))

if args.fuzzy:
    get_selection(legend, "", lines=30, font=args.dmenu_font)
else:
    try:
        show_text_dialog(text=legend, title="keybindings")
    except ValueError:
        print("Canceled")
