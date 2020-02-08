import os
import sys
import subprocess

from libtmux import Server
import dmenu
import notify2
from notify2 import URGENCY_CRITICAL
import redis


notify2.init("remote_docker_logs")
r = redis.Redis(host='localhost', port=6379, db=0)

# TODO: extract to utility function
if r.get("job_vpn_status").decode() != "up":
    n = notify2.Notification("[docker]", "VPN is off, turn it on and retry")
    n.set_urgency(URGENCY_CRITICAL)
    n.set_timeout(5000)
    n.show()
    sys.exit(1)

remote_find_cmd = "find @jobInfraRemoteDockerLogsRoot@/ -maxdepth 1 -size +0 -type f | grep -v gz"

logs_task = subprocess.Popen(
    "@sshBinary@ @jobInfraLogsHost@ {0}".format(remote_find_cmd),
    shell=True, stdout=subprocess.PIPE
)
logs_result = logs_task.stdout.read().decode().strip().split("\n")

selected_log = dmenu.show(logs_result, prompt="view", case_insensitive=True, lines=10)
if not selected_log:
    sys.exit(1)

view_log_cmd = "@sshBinary@ @jobInfraLogsHost@ 'tail -f {0}'".format(selected_log)

tmux_server = Server()
tmux_session = tmux_server.find_where({ "session_name": "@tmuxDefaultSession@" })
ssh_window = tmux_session.new_window(attach=True, window_name=selected_log.split("/")[-1], window_shell=view_log_cmd)
