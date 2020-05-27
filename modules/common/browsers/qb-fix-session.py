import argparse
import os
import shutil

import yaml

SESSIONS_PATH = "{0}/.local/share/qutebrowser/sessions/".format(os.environ["HOME"])
DEFAULT_SESSION = "default.yml"
BACKUP_SUFFIX = ".backup"

CURRENT_TAB_DICT_PATCH = {
    "scroll-pos": {
        "x": 0,
        "y": 0
    },
    "active": True,
    "zoom": 1.0
}


parser = argparse.ArgumentParser(description="Fix qutebrowser default session contents.")
parser.add_argument("--session-path", "-s", dest="session_path", default=SESSIONS_PATH+DEFAULT_SESSION,
                    help="org-capture template key")

args = parser.parse_args()

# backing up session
shutil.copyfile(args.session_path, args.session_path + BACKUP_SUFFIX)

session = None
with open(SESSIONS_PATH + DEFAULT_SESSION, "r") as s:
    session = yaml.load(s)
    for window in session["windows"]:
        window_tabs = window["tabs"]
        for tab in window_tabs:
            tab_history = tab["history"]
            fixed_history = []
            for item in tab_history:
                fixed_item = item
                if item["title"].startswith("Error loading"):
                    continue
                if item["url"].startswith("data:text/html"):
                    continue
                original_url = item.get("original-url")
                if original_url and original_url.startswith("data:text/html"):
                    del fixed_item["original-url"]

                fixed_history.append(fixed_item)
            if fixed_history:
                fixed_history[-1].update(CURRENT_TAB_DICT_PATCH)
            tab["history"] = fixed_history

if session:
    with open(SESSIONS_PATH + DEFAULT_SESSION + ".fixed", "w") as s:
        s.write(yaml.dump(session))
