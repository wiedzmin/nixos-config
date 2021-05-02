import os
import sys

import redis

from pystdlib.uishim import get_selection, show_text_dialog
from pystdlib import shell_cmd


r = redis.Redis(host='localhost', port=6379, db=0)
extra_hosts_data = json.loads(r.get("net/extra_hosts"))

CONTAINER_TRAITS = {
    "name": '{{index (split .Name \\"/\\") 1}}',
    "created": '{{.Created}}',
    "path + args": '{{.Path}} :: {{.Args}}',
    "stats": '{{println .State.Status}} {{.State.StartedAt}} <--> {{println .State.FinishedAt}} restarts: {{.RestartCount}}',
    "ports": '{{range $port, $mappings :=.NetworkSettings.Ports}}{{$port}} --> {{range $ifnum, $ifdef:=$mappings}}{{$ifnum}}) {{$ifdef.HostIp}}:{{$ifdef.HostPort}}{{end}}{{end}}',
    "mounts": '{{range $i, $mountpoint :=.Mounts}}{{with $mountpoint}}{{.Type}} {{.Destination}} --> {{.Source}} RW:{{.RW}}{{end}}{{end}}',
    "env": '{{range $entry :=.Config.Env}}{{with $entry}}{{println .}}{{end}}{{end}}',
    "cmd": '{{index .Config.Cmd 0}}',
    "image": '{{.Config.Image}}',
    "volumes": '{{range $vol, $data :=.Config.Volumes}}{{$vol}}: {{$data}}{{end}}',
    "entrypoint": '{{index .Config.Entrypoint 0}}',
    "labels": '{{range $name, $value :=.Config.Labels}}{{$name}}: {{println $value}}{{end}}',
    "net: ip": '{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.IPAddress}}{{end}}',
    "net: gateway": '{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.Gateway}}{{end}}',
    "net: names": '{{range $network, $settings :=.NetworkSettings.Networks}}{{$network}}/{{println $settings.Aliases}}{{end}}'
}

CONTAINER_STATUSES = [
    "alive",
    "all"
]


parser = argparse.ArgumentParser(description="Containers traits")
parser.add_argument('--dmenu-font', dest="dmenu_font", type=str, help="Dmenu font")
args = parser.parse_args()

hostnames = []

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

container_status = get_selection(CONTAINER_STATUSES, "status", case_insensitive=True, lines=3, font=args.dmenu_font)
if not container_status:
    sys.exit(1)

select_container_result = shell_cmd(
    f"docker ps {'-a ' if container_status == 'all' else ''}--format '{{.Names}}'",
    split_output="\n")

selected_container = get_selection(select_container_result, "container", case_insensitive=True, lines=10, font=args.dmenu_font)
selected_trait = get_selection(CONTAINER_TRAITS.keys(), "inspect", case_insensitive=True, lines=10, font=args.dmenu_font)

docker_inspect_cmd = f'docker inspect {selected_container} --format "{CONTAINER_TRAITS[selected_trait]}"'
inspect_result = shell_cmd(["xsel", "-ib"], universal_newlines=True, input=docker_inspect_cmd
                           env={"DOCKER_HOST": os.environ['DOCKER_HOST']})

show_text_dialog(text=inspect_result)
