import json
import os

import redis

from pystdlib.uishim import show_text_dialog


# TODO: add parameter for various search approaches
# Either full list (as is) or dmenu noop choices should be displayed
# dmenu approach is essential for fuzzy recalling

r = redis.Redis(host='localhost', port=6379, db=0)

if not os.path.exists("@keybindingsCachePath@"):
    keybindings = json.loads(r.get("wm/keybindings"))

    legend = []
    for key, meta in keybindings.items():
        desktop = None
        if type(meta) == str:
            cmd = meta
        else:
            cmd = meta.get("cmd")
            desktop = meta.get("desktop", None)
        ands_index = cmd.rfind("&&")
        if ands_index == -1:
            cmd_result = cmd
        else:
            cmd_result = cmd[:ands_index]
        cmd_clean = cmd_result[cmd_result.rfind("/")+1:].strip('"/').replace('"', '')
        maybe_desktop = " (" + desktop + ")" if desktop else ""
        legend.append('{key}: {cmd_clean}{maybe_desktop}\n')

show_text_dialog(text=legend, title="keybindings", keep=True, path="@keybindingsCachePath@")
