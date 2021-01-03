import json
import os

import redis

from pystdlib.uishim import show_text_dialog


# TODO: add parameter for various search approaches
# Either full list (as is) or dmenu noop choices should be displayed
# dmenu approach is essential for fuzzy recalling

r = redis.Redis(host='localhost', port=6379, db=0)
legend = []


def format_modebinding(name, binding=None):
    mode_name = name.split("-")[0].strip()
    mode_binding = binding
    if not mode_binding:
        mode_binding = "-"
    if type(mode_binding) == list:
        mode_binding = "+".join(binding)
    return f'MODE: {mode_name} ({mode_binding}):'


def format_keybinding(kb):
    cmd_clean = kb["cmd"]
    if "/nix/store" in cmd_clean:
        cmd_clean = cmd_clean.split("/bin/")[-1]
    return f'    [{"+".join(kb["key"])}]: {cmd_clean}'


if not os.path.exists("@keybindingsCachePath@"):
    keybindings = json.loads(r.get("wm/keybindings"))
    modebindings = json.loads(r.get("wm/modebindings"))

    keybindings_by_mode = {}
    dangling_keybindings_by_mode = {}

    for kb in keybindings:
        mode = modebindings.get(kb["mode"], None)
        if kb["mode"] == "root" or mode:
            keybindings_by_mode.setdefault(kb["mode"], []).append(kb)
        else:
            dangling_keybindings_by_mode.setdefault(kb["mode"], []).append(kb)

    for mode_name in keybindings_by_mode:
        legend.append(format_modebinding(mode_name,
                                         binding=modebindings[mode_name]
                                         if mode_name != "root" else "non-prefix"))
        for kb in keybindings_by_mode[mode_name]:
            legend.append(format_keybinding(kb))

    if len(dangling_keybindings_by_mode):
        legend.append("================== DANGLING KEYBINDINGS ==================")
        for mode_name in dangling_keybindings_by_mode:
            legend.append(format_modebinding(mode_name))
            for kb in dangling_keybindings_by_mode[mode_name]:
                legend.append(format_keybinding(kb))

else:
    with open("@keybindingsCachePath@", "r") as f:
        legend = f.readlines()

try:
    show_text_dialog(text=legend, title="keybindings", keep=True, path="@keybindingsCachePath@")
except ValueError:
    print("Canceled")
