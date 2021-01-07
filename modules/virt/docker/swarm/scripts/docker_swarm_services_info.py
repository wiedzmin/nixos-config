import json
import os
import sys

import redis

from pystdlib.uishim import get_selection, notify, show_text_dialog, URGENCY_CRITICAL
from pystdlib.shell import tmux_create_window
from pystdlib import shell_cmd


service_modes = [
    "status",
    "logs"
]


r = redis.Redis(host='localhost', port=6379, db=0)
extra_hosts_data = json.loads(r.get("net/extra_hosts"))

swarm_meta = json.loads(r.get("virt/swarm_meta"))
swarm = get_selection(swarm_meta.keys(), "swarm", case_insensitive=True, lines=5, font="@wmFontDmenu@")
if not swarm:
    notify("[virt]", "No swarm selected")
    sys.exit(0)

swarm_host = swarm_meta[swarm]
os.environ["DOCKER_HOST"] = f"ssh://{swarm_host}"
host_meta = extra_hosts_data.get(swarm_host, None)
if not host_meta:
    notify("[docker]", f"Host '{swarm_host}' not found", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

host_vpn = host_meta.get("vpn", None)
if host_vpn:
    shell_cmd(f"vpnctl --start {host_vpn}")

services_meta = shell_cmd("docker service ls --format '{{.Name}} | {{.Mode}} | {{.Replicas}} | {{.Image}}'",
                          split_output="\n")

selected_service_meta = get_selection(services_meta, "service", case_insensitive=True, lines=10, font="@wmFontDmenu@")
selected_service_name = selected_service_meta.split("|")[0].strip()
selected_mode = get_selection(service_modes, "show", case_insensitive=True, lines=5, font="@wmFontDmenu@")

service_status = shell_cmd(f"docker service ps {selected_service_name}", split_output="\n")

if selected_mode == "status":
    show_text_dialog(text=service_status)
elif selected_mode == "logs":
    service_running_tasks_items = [task.split() for task in service_status if "Running" in task]
    task_mappings = dict([(task_meta[1], task_meta[0]) for task_meta in service_running_tasks_items])
    selected_task = get_selection(list(task_mappings.keys()) + [selected_service_name], "task",
                                  case_insensitive=True, lines=10, font="@wmFontDmenu@")
    if not selected_task:
        sys.exit(1)

    task_or_service = task_mappings.get(selected_task) if selected_task in task_mappings else selected_service_name
    show_log_cmd = f"DOCKER_HOST={os.environ['DOCKER_HOST']} docker service logs --follow {task_or_service}"
    tmux_create_window(show_log_cmd,
                       session_name=host_meta.get("tmux", "@tmuxDefaultSession@"),
                       window_title=f"{selected_task} logs")
