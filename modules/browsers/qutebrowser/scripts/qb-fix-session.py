import argparse
import os
import shutil

from yaml import dump, load, Loader

from pystdlib.browser import qutebrowser_fix_session

SESSIONS_PATH = f'{os.environ["HOME"]}/.local/share/qutebrowser/sessions/'
DEFAULT_SESSION = "default.yml"
BACKUP_SUFFIX = ".backup"


parser = argparse.ArgumentParser(description="Fix qutebrowser default session contents.")
parser.add_argument("--session-path", "-s", dest="session_path", default=SESSIONS_PATH+DEFAULT_SESSION,
                    help="org-capture template key")

args = parser.parse_args()

# backing up session
shutil.copyfile(args.session_path, args.session_path + BACKUP_SUFFIX)

session = None
with open(SESSIONS_PATH + DEFAULT_SESSION, "r") as s:
    session = qutebrowser_fix_session(load(s, Loader=Loader))

if session:
    with open(SESSIONS_PATH + DEFAULT_SESSION + ".fixed", "w") as s:
        s.write(dump(session))
