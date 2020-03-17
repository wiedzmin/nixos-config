import json
import os
import subprocess

import redis


r = redis.Redis(host='localhost', port=6379, db=0)

keybindings = json.loads(r.get("wm/keybindings"))

legend = []
for key, cmd in keybindings.items():
    ands_index = cmd.rfind("&&")
    if ands_index == -1:
        cmd_result = cmd
    else:
        cmd_result = cmd[:ands_index]
    legend.append('{0}: {1}\n'.format(key, cmd_result[cmd_result.rfind("/")+1:].strip('"/').replace('"', '')))

with open("/tmp/wm_keybindings", "w") as f:
    f.writelines(legend)

show_dialog_task = subprocess.Popen("@yadBinary@ --filename /tmp/wm_keybindings --text-info",
                                    shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
show_dialog_task.wait()
os.remove("/tmp/wm_keybindings")
