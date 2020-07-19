import os
import sys
import subprocess

import dmenu

port_cmd_mapping = {
    "80": "@defaultBrowser@",
    "8080": "@defaultBrowser@",
    "8000": "@defaultBrowser@"
}

@pythonPatchUIShim@

ip_address_format = "{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.IPAddress}}{{end}}"
ports_format = "{{range $port, $mappings :=.NetworkSettings.Ports}}{{$port}}{{end}}"

if "DOCKER_HOST" in os.environ:
    del os.environ["DOCKER_HOST"] # ensure we cosidering only local containers

select_container_task = subprocess.Popen("docker ps --format '{{.Names}}'", shell=True, stdout=subprocess.PIPE)
select_container_result = select_container_task.stdout.read().decode().split("\n")

selected_container = dmenu.show(select_container_result, prompt="container", case_insensitive=True, lines=10, font="@wmFontDmenu@")
if not selected_container:
    sys.exit(1)

container_ip_task = subprocess.Popen(f"docker inspect {selected_container} --format='{ip_address_format}'",
                                     shell=True, stdout=subprocess.PIPE)
container_ip_result = container_ip_task.stdout.read().decode().strip()

container_ports_task = subprocess.Popen(f"docker inspect {selected_container} --format='{ports_format}'",
                                        shell=True, stdout=subprocess.PIPE)
container_ports_result = container_ports_task.stdout.read().decode().strip().split("\n")

port_result = None

for port in container_ports_result:
    port_number = port.split("/")[0]
    if port_number in port_cmd_mapping:
        port_result = port_number
        break

if not port_result:
    notify("[docker]", f"No suitable port between exposed:\n{'\n'.join(container_ports_result)}", timeout=5000)
    sys.exit(0)

open_cmd = f"{port_cmd_mapping[port_result]} http://{container_ip_result}:{port_result}"
subprocess.run(open_cmd.split())
