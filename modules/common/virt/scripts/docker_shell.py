import os
import subprocess

from pystdlib.uishim import get_selection
import redis
from libtmux import Server

hostnames = []

r = redis.Redis(host='localhost', port=6379, db=0)
extra_hosts_data = json.loads(r.get("net/extra_hosts"))

with open("/etc/hosts", "r") as hosts:
    for host in hosts:
        host_list = list(reversed(host.strip(";\n").split()))
        if host_list:
            hostnames.extend(host_list[:-1])

hostnames = sorted(list(set(hostnames)))

hostname = get_selection(hostnames, "host", case_insensitive=True, lines=10, font="@wmFontDmenu@")

host_meta = extra_hosts_data.get(hostname, None)
if not host_meta:
    notify("[docker]", f"Host '{hostname}' not found", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

if hostname == "localhost":
    os.environ["DOCKER_HOST"] = "unix:///var/run/docker.sock"
else:
    os.environ["DOCKER_HOST"] = f"ssh://{hostname}"
    host_vpn = host_meta.get("vpn", None)
    if host_vpn:
        vpn_start_task = subprocess.Popen(f"vpnctl --start {host_vpn}",
                                          shell=True, stdout=subprocess.PIPE)
        assert vpn_start_task.wait() == 0

select_container_task = subprocess.Popen("docker ps --format '{{.Names}}'", shell=True, stdout=subprocess.PIPE)
select_container_result = select_container_task.stdout.read().decode().split("\n")

selected_container = get_selection(select_container_result, "container", case_insensitive=True, lines=10, font="@wmFontDmenu@")
if not selected_container:
    sys.exit(1)

get_shell_cmd = f"export DOCKER_HOST={os.environ['DOCKER_HOST']} && docker exec -it {selected_container} @defaultContainerShell@"
tmux_server = Server()
session_name =  host_meta.get("tmux", "@tmuxDefaultSession@")
tmux_session = tmux_server.find_where({ "session_name": session_name })
docker_shell_window = tmux_session.new_window(attach=True, window_name=f"{selected_container} shell",
                                              window_shell=get_shell_cmd)
