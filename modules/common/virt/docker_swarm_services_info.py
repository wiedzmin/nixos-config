import json
import os
import subprocess
import sys

import dmenu
import redis
from libtmux import Server


service_modes = [
    "status",
    "logs"
]

@pythonPatchNotify@

r = redis.Redis(host='localhost', port=6379, db=0)

swarm_meta = json.loads(r.get("virt/swarm_meta"))
swarm = dmenu.show(swarm_meta.keys(), prompt="swarm", case_insensitive=True, lines=5)
if not swarm:
    notify("[virt]", "No swarm selected")
    sys.exit(0)

swarm_host = swarm_meta[swarm]
os.environ["DOCKER_HOST"] = f"ssh://{swarm_host}"
vpn_meta = json.loads(r.get("net/hosts_vpn"))
host_vpn = vpn_meta.get(swarm_host, None)
if host_vpn:
    vpn_is_up = r.get(f"vpn/{host_vpn}/is_up").decode() == "yes"
    if not vpn_is_up:
        vpn_start_task = subprocess.Popen(f"vpnctl --start {host_vpn}",
                                          shell=True, stdout=subprocess.PIPE)
        assert vpn_start_task.wait() == 0

select_service_task = subprocess.Popen("docker service ls --format '{{.Name}} | {{.Mode}} | {{.Replicas}} | {{.Image}}'",
                                       shell=True, stdout=subprocess.PIPE)
select_service_result = select_service_task.stdout.read().decode().split("\n")

selected_service_meta = dmenu.show(select_service_result, prompt="service", case_insensitive=True, lines=10)
selected_service_name = selected_service_meta.split("|")[0].strip()
selected_mode = dmenu.show(service_modes, prompt="show", case_insensitive=True, lines=5)

get_service_status_task = subprocess.Popen(f"docker service ps {selected_service_name}",
                                           shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
get_service_status_result = get_service_status_task.stdout.read().decode()

if selected_mode == "status":
    with open("/tmp/docker_swarm_service_info", "w") as f:
        f.write(get_service_status_result)
    show_dialog_task = subprocess.Popen("yad --filename /tmp/docker_swarm_service_info --text-info",
                                        shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    show_dialog_task.wait()
    os.remove("/tmp/docker_swarm_service_info")
elif selected_mode == "logs":
    service_running_tasks_items = [task.split() for task in get_service_status_result.split("\n")
                                   if "Running" in task]
    task_mappings = dict([(task_meta[1], task_meta[0]) for task_meta in service_running_tasks_items])
    selected_task = dmenu.show(list(task_mappings.keys()) + [selected_service_name],
                               prompt="task", case_insensitive=True, lines=10)
    if not selected_task:
        sys.exit(1)

    tmux_sessions = json.loads(r.get("tmux/extra_hosts"))
    session_name = tmux_sessions.get(swarm_host) or "@tmuxDefaultSession@"
    task_or_service = task_mappings.get(selected_task) if selected_task in task_mappings else selected_service_name
    show_log_cmd = f"DOCKER_HOST={os.environ['DOCKER_HOST']} docker service logs --follow {task_or_service}"
    tmux_server = Server()
    tmux_session = tmux_server.find_where({ "session_name": session_name })
    docker_task_log_window = tmux_session.new_window(attach=True, window_name=f"{selected_task} logs",
                                                     window_shell=show_log_cmd)
