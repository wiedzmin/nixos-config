import os
import json

import redis

from pystdlib.uishim import get_selection
from pystdlib.shell import tmux_create_window
from pystdlib import shell_cmd


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
        shell_cmd(f"vpnctl --start {host_vpn}")

container_names = shell_cmd("docker ps --format '{{.Names}}'", split_output="\n")
selected_container = get_selection(container_names, "container", case_insensitive=True, lines=10, font="@wmFontDmenu@")
if not selected_container:
    sys.exit(1)

get_shell_cmd = f"export DOCKER_HOST={os.environ['DOCKER_HOST']} && docker exec -it {selected_container} @defaultContainerShell@"
tmux_create_window(get_shell_cmd,
                   session_name=host_meta.get("tmux", "@tmuxDefaultSession@"),
                   window_title=f"{selected_container} shell")
