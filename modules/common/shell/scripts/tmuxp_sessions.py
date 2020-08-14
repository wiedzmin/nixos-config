import os
from pystdlib.uishim import get_selection

configs = []
TMUXP_SESSIONS_PATH = f'{os.getenv("HOME")}/tmuxp'


for root, dirs, files in os.walk(TMUXP_SESSIONS_PATH):
    for file in files:
        if file.endswith(".yml"):
            configs.append(os.path.splitext(file)[0])

result = get_selection(sorted(configs), 'config', lines=10, font="@wmFontDmenu@")

if result:
    os.system(f"tmuxp load -y -d {TMUXP_SESSIONS_PATH}/{result}.yml")
