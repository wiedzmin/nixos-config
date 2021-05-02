import argparse
import json
import os

import redis

from pystdlib.uishim import get_selection
from pystdlib.shell import tmux_create_window
from pystdlib import shell_cmd


hostnames = []

parser = argparse.ArgumentParser(description="Searchengines")
parser.add_argument('--tmux-session', dest="tmux_session", default="main", type=str, help="Fallback tmux session name")
parser.add_argument('--shell', dest="tmux_session", default="/bin/bash", type=str, help="Default shell inside container")
parser.add_argument('--dmenu-font', dest="dmenu_font", type=str, help="Dmenu font")
args = parser.parse_args()


r = redis.Redis(host='localhost', port=6379, db=0)
extra_hosts_data = json.loads(r.get("net/extra_hosts"))

with open("/etc/hosts", "r") as hosts:
    for host in hosts:
        host_list = list(reversed(host.strip(";\n").split()))
        if host_list:
            hostnames.extend(host_list[:-1])
hostnames = sorted(list(set(hostnames)))

hostname = get_selection(hostnames, "host", case_insensitive=True, lines=10, font=args.dmenu_font)
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
selected_container = get_selection(container_names, "container", case_insensitive=True, lines=10, font=args.dmenu_font)
if not selected_container:
    sys.exit(1)

get_shell_cmd = f"export DOCKER_HOST={os.environ['DOCKER_HOST']} && docker exec -it {selected_container} {args.shell}"
tmux_create_window(get_shell_cmd,
                   session_name=host_meta.get("tmux", args.tmux_session),
                   window_title=f"{selected_container} shell")
