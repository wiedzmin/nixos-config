import os
import re
import sys

from pystdlib.uishim import get_selection, notify

pattern = "\(.*\)"
deps_dict = {}


go_mod_path = os.getcwd() + "/go.mod"
if not (os.path.exists(go_mod_path) and os.path.isfile(go_mod_path)):
    notify("[modedit]", "No go.mod found", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

with open(go_mod_path, "r") as f:
    deps_list = re.search(pattern, f.read(), re.DOTALL).group(0).split("\n")[1:-1]
    deps_list = [dep for dep in deps_list if not dep.startswith("//")]
    deps_list = [dep for dep in deps_list if dep.startswith("\t")]
    for dep in deps_list:
        deps_dict[dep.split()[0]] = dep.split()[1]

dep_path = get_selection(deps_dict.keys(), "replace", case_insensitive=True, lines=10, font="@wmFontDmenu@")

if not dep_path:
    notify("[modedit]", "Nothing selected", timeout=5000)
    sys.exit(0)

dep_path_local = "@globalWorkspaceRoot@/" + dep_path
if dep_path_local.endswith(".git"):
    dep_path_local = dep_path_local[:-4]

if not (os.path.exists(dep_path_local) and os.path.isdir(dep_path_local)):
    notify("[modedit]", "No dependency repo found locally", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

with open(go_mod_path, "a") as f:
    f.write(f"\nreplace {dep_path} => {dep_path_local}")
