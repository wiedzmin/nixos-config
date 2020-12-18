import os
import sys

from pystdlib.uishim import get_selection, notify
from pystdlib import shell_cmd


port_cmd_mapping = {
    "80": "@defaultBrowser@",
    "8080": "@defaultBrowser@",
    "8000": "@defaultBrowser@"
}


ip_address_format = "{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.IPAddress}}{{end}}"
ports_format = "{{range $port, $mappings :=.NetworkSettings.Ports}}{{$port}}{{end}}"

if "DOCKER_HOST" in os.environ:
    del os.environ["DOCKER_HOST"] # ensure we cosidering only local containers

container_names = shell_cmd("docker ps --format '{{.Names}}'", split_output="\n")
selected_container = get_selection(container_names, "container", case_insensitive=True, lines=10, font="@wmFontDmenu@")
if not selected_container:
    sys.exit(1)

container_ip = shell_cmd(f"docker inspect {selected_container} --format='{ip_address_format}'")
container_ports = shell_cmd(f"docker inspect {selected_container} --format='{ports_format}'", split_output="\n")

port_result = None

for port in container_ports_result:
    port_number = port.split("/")[0]
    if port_number in port_cmd_mapping:
        port_result = port_number
        break

if not port_result:
    notify("[docker]", f"No suitable port between exposed:\n{'\n'.join(container_ports_result)}", timeout=5000)
    sys.exit(0)

shell_cmd(f"{port_cmd_mapping[port_result]} http://{container_ip_result}:{port_result}")
