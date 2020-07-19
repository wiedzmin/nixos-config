import subprocess

import os
import sys

import dmenu
import redis


r = redis.Redis(host='localhost', port=6379, db=0)

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

hostnames = []

with open("/etc/hosts", "r") as hosts:
    for host in hosts:
        host_list = list(reversed(host.strip(";\n").split()))
        if host_list:
            hostnames.extend(host_list[:-1])

hostnames = sorted(list(set(hostnames)))

hostname = dmenu.show(hostnames, prompt="host", case_insensitive=True, lines=10, font="@wmFontDmenu@")

if hostname == "localhost":
    os.environ["DOCKER_HOST"] = "unix:///var/run/docker.sock"
else:
    os.environ["DOCKER_HOST"] = f"ssh://{hostname}"
    vpn_meta = json.loads(r.get("net/hosts_vpn"))
    host_vpn = vpn_meta.get(host, None)
    if host_vpn:
        vpn_start_task = subprocess.Popen(f"vpnctl --start {host_vpn}",
                                          shell=True, stdout=subprocess.PIPE)
        assert vpn_start_task.wait() == 0

container_status = dmenu.show(CONTAINER_STATUSES, prompt="status", case_insensitive=True, lines=3, font="@wmFontDmenu@")
if not container_status:
    sys.exit(1)

docker_ps_cmd = f"docker ps {'-a ' if container_status == 'all' else ''}--format '{{.Names}}'"
select_container_task = subprocess.Popen(docker_ps_cmd, shell=True, stdout=subprocess.PIPE)
select_container_result = select_container_task.stdout.read().decode().split("\n")

selected_container = dmenu.show(select_container_result, prompt="container", case_insensitive=True, lines=10, font="@wmFontDmenu@")
selected_trait = dmenu.show(CONTAINER_TRAITS.keys(), prompt="inspect", case_insensitive=True, lines=10, font="@wmFontDmenu@")

docker_inspect_cmd = f'docker inspect {selected_container} --format "{CONTAINER_TRAITS[selected_trait]}"'

# FIXME: remove nested "formats"
subprocess.run(["xsel", "-ib"], universal_newlines=True,
               input=f"export DOCKER_HOST={os.environ['DOCKER_HOST']} && {docker_inspect_cmd}")

get_traits_task = subprocess.Popen(docker_inspect_cmd, shell=True, stdout=subprocess.PIPE)
with open("/tmp/docker_traits", "w") as f:
    f.write(get_traits_task.stdout.read().decode())

show_dialog_task = subprocess.Popen("yad --filename /tmp/docker_traits --text-info",
                                    shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
show_dialog_task.wait()
os.remove("/tmp/docker_traits")
