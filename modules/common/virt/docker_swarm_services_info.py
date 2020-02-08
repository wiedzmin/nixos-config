import os
import subprocess
import sys

import dmenu
import notify2
from notify2 import URGENCY_CRITICAL
import redis
from libtmux import Server


service_modes = [
    "status",
    "logs"
]

notify2.init("docker_swarm_services_info")
r = redis.Redis(host='localhost', port=6379, db=0)

os.environ["DOCKER_HOST"] = "ssh://{0}".format("@jobInfraSwarmLeader@")
# TODO: extract to utility function
if r.get("job_vpn_status").decode() != "up":
    n = notify2.Notification("[docker]", "VPN is off, turn it on and retry")
    n.set_urgency(URGENCY_CRITICAL)
    n.set_timeout(5000)
    n.show()
    sys.exit(1)

select_service_task = subprocess.Popen("@dockerBinary@ service ls --format '{{.Name}} | {{.Mode}} | {{.Replicas}} | {{.Image}}'",
                                       shell=True, stdout=subprocess.PIPE)
select_service_result = select_service_task.stdout.read().decode().split("\n")

selected_service_meta = dmenu.show(select_service_result, prompt="service", case_insensitive=True, lines=10)
selected_service_name = selected_service_meta.split("|")[0].strip()
selected_mode = dmenu.show(service_modes, prompt="show", case_insensitive=True, lines=5)

get_service_status_task = subprocess.Popen("@dockerBinary@ service ps {0}".format(selected_service_name),
                                           shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
get_service_status_result = get_service_status_task.stdout.read().decode()

if selected_mode == "status":
    with open("/tmp/docker_swarm_service_info", "w") as f:
        f.write(get_service_status_result)
    show_dialog_task = subprocess.Popen("@yadBinary@ --filename /tmp/docker_swarm_service_info --text-info",
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

    show_log_cmd = "{0}@dockerBinary@ service logs --follow {1}".format(
        "export DOCKER_HOST={0} && ".format(os.environ["DOCKER_HOST"]) if os.environ["DOCKER_HOST"] else "",
        task_mappings.get(selected_task) if selected_task in task_mappings else selected_service_name)
    tmux_server = Server()
    tmux_session = tmux_server.find_where({ "session_name": "@tmuxDefaultSession@" })
    docker_task_log_window = tmux_session.new_window(
        attach=True,
        window_name="{0} logs".format(selected_task),
        window_shell=show_log_cmd)
