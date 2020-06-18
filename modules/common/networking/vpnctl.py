import argparse
import json
import subprocess
import sys
import time

import notify2
import redis

from notify2 import URGENCY_NORMAL, URGENCY_CRITICAL


parser = argparse.ArgumentParser(description="Manage enabled VPN services.")
parser.add_argument("--start", dest="vpn_service_tag", help="Start selected VPN service")
parser.add_argument("--stop-running", dest="stop_running", action="store_true",
                   default=False, help="Stop currently running VPN service")

args = parser.parse_args()
notify2.init("vpnctl")

r = redis.Redis(host='localhost', port=6379, db=0)
vpn_meta = json.loads(r.get("net/vpn_meta"))

def stop_running(omit=None):
    devdns_stop_task = subprocess.Popen("systemctl stop docker-devdns.service",
                                        shell=True, stdout=subprocess.PIPE)
    assert devdns_stop_task.wait() == 0
    if omit:
        del vpn_meta[omit]
    stop_cmds = {vpn: vpn_meta[vpn]["down"] for vpn in vpn_meta}
    for vpn, cmd in stop_cmds.items():
        vpn_up = r.get("vpn/{0}/is_up".format(vpn)).decode() == "yes"
        if vpn_up:
            vpn_stop_task = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE)


if args.vpn_service_tag:
    service = vpn_meta.get(args.vpn_service_tag, None)
    if not service:
        n = notify2.Notification("[VPN]", "Cannot find {0} service".format(args.vpn_service_tag))
        n.set_urgency(URGENCY_CRITICAL)
        n.set_timeout(5000)
        n.show()
        sys.exit(1)
    stop_running(omit=args.vpn_service_tag)
    vpn_start_task = subprocess.Popen(service["up"], shell=True, stdout=subprocess.PIPE)
    assert vpn_start_task.wait() == 0
    time.sleep(3)
    n = notify2.Notification("[VPN]", "Started {0} service".format(args.vpn_service_tag))
    n.set_urgency(URGENCY_NORMAL)
    n.set_timeout(5000)
    n.show()
elif args.stop_running:
    stop_running()
