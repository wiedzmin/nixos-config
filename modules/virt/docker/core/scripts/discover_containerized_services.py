import argparse
import os
import sys

from pystdlib.uishim import get_selection_rofi, notify
from pystdlib import shell_cmd


parser = argparse.ArgumentParser(description="DBMS connectivity")
parser.add_argument('--browser', dest="default_browser", type=str, help="Default browser")
parser.add_argument('--fallback-browser', dest="fallback_browser", type=str, help="Fallback browser")
parser.add_argument('--selector-font', dest="selector_font", type=str, help="Selector font")
args = parser.parse_args()

port_cmd_mapping = {
    "80": args.default_browser,
    "8080": args.default_browser,
    "8000": args.default_browser
}

if not (args.default_browser and args.fallback_browser):
    notify(f"[containers]", f"Browsers not set, exiting...", urgency=URGENCY_CRITICAL, timeout=5000)
    sys.exit(1)

ip_address_format = "{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.IPAddress}}{{end}}"
ports_format = "{{range $port, $mappings :=.NetworkSettings.Ports}}{{$port}}{{end}}"

if "DOCKER_HOST" in os.environ:
    del os.environ["DOCKER_HOST"] # ensure we cosidering only local containers

container_names = shell_cmd("docker ps --format '{{.Names}}'", split_output="\n")
selected_container = get_selection_rofi(container_names, "container")
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
