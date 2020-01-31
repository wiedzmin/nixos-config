import os
import dmenu

configs = []
TMUXP_SESSIONS_PATH = "{0}/tmuxp".format(os.getenv("HOME"))


for root, dirs, files in os.walk(TMUXP_SESSIONS_PATH):
    for file in files:
        if file.endswith(".yml"):
            configs.append(os.path.splitext(file)[0])

result = dmenu.show(sorted(configs), prompt='config', lines=10)
if result:
    os.system("tmuxp load -y -d {0}/{1}.yml".format(TMUXP_SESSIONS_PATH, result))
