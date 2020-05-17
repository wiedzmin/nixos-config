import os
import re
import sys

import dmenu
import notify2
from notify2 import URGENCY_NORMAL, URGENCY_CRITICAL


pattern = "\(.*\)"
deps_dict = {}

notify2.init("modedit")

go_mod_path = os.getcwd() + "/go.mod"
if not (os.path.exists(go_mod_path) and os.path.isfile(go_mod_path)):
    n = notify2.Notification("[modedit]", "No go.mod found")
    n.set_urgency(URGENCY_CRITICAL)
    n.set_timeout(5000)
    n.show()
    sys.exit(1)

with open(go_mod_path, "r") as f:
    deps_list = re.search(pattern, f.read(), re.DOTALL).group(0).split("\n")[1:-1]
    for dep in deps_list:
        deps_dict[dep.split()[0]] = dep.split()[1]

dep_path = dmenu.show(deps_dict.keys(), prompt="replace",
                      case_insensitive=True, lines=10)

if not dep_path:
    n = notify2.Notification("[modedit]", "Nothing selected")
    n.set_urgency(URGENCY_NORMAL)
    n.set_timeout(5000)
    n.show()
    sys.exit(0)

dep_path_local = "@globalWorkspaceRoot@/" + dep_path
if dep_path_local.endswith(".git"):
    dep_path_local = dep_path_local[:-4]

if not (os.path.exists(dep_path_local) and os.path.isdir(dep_path_local)):
    n = notify2.Notification("[modedit]", "No dependency repo found locally")
    n.set_urgency(URGENCY_CRITICAL)
    n.set_timeout(5000)
    n.show()
    sys.exit(1)

with open(go_mod_path, "a") as f:
    f.write("\nreplace {0} => {1}".format(dep_path, dep_path_local))
