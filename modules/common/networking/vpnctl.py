import argparse
import json
import subprocess
import sys
import time
import os

import redis

IPV4_STATUS_PATH = "/proc/sys/net/ipv4/conf/"

parser = argparse.ArgumentParser(description="Manage enabled VPN services.")
parser.add_argument("--start", dest="vpn_service_tag", help="Start selected VPN service")
parser.add_argument("--stop-running", dest="stop_running", action="store_true",
                   default=False, help="Stop currently running VPN service")

args = parser.parse_args()

@pythonPatchNotify@

r = redis.Redis(host='localhost', port=6379, db=0)
vpn_meta = json.loads(r.get("net/vpn_meta"))


def start_service(name, meta):
    result = False
    if meta["type"] == "ovpn":
        tun_path = IPV4_STATUS_PATH + meta["dev"]
        vpn_start_task = subprocess.Popen(meta["up"], shell=True, stdout=subprocess.PIPE)
        if vpn_start_task.wait() == 0:
            while not os.path.exists(tun_path):
                time.sleep(1)
            r.set(f"vpn/{name}/is_up", "yes")
            result = True
            notify("[VPN]", f"`{name}` is up")
        else:
            r.set(f"vpn/{name}/is_up", "unk")
            result = False
            notify("[VPN]", f"error starting `{name}`", urgency=URGENCY_CRITICAL, timeout=5000)
    elif meta["type"] == "ipsec":
        vpn_start_task = subprocess.Popen(meta["up"], shell=True, env={'LANGUAGE':'en_US.en'},
                                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        vpn_start_result = vpn_start_task.wait()
        if vpn_start_result != 0:
            vpn_stderr = vpn_start_task.stderr.read().decode().strip().split()
            if "is already active" in " ".join(vpn_stderr):
                r.set(f"vpn/{name}/is_up", "yes")
                result = True
                notify("[VPN]", f"`{name}` is up")
            else:
                r.set(f"vpn/{name}/is_up", "unk")
                result = False
                notify("[VPN]", f"error starting `{name}`", urgency=URGENCY_CRITICAL, timeout=5000)
        else:
            r.set(f"vpn/{name}/is_up", "yes")
            result = True
            notify("[VPN]", f"`{name}` is up")
    else:
        result = False
        notify("[VPN]", f"Unknown service type: {meta['type']} service", urgency=URGENCY_CRITICAL, timeout=5000)
    return result


def stop_service(name, meta):
    result = False
    if meta["type"] == "ovpn":
        tun_path = IPV4_STATUS_PATH + meta["dev"]
        vpn_stop_task = subprocess.Popen(meta["down"], shell=True, stdout=subprocess.PIPE)
        if vpn_stop_task.wait() == 0:
            while os.path.exists(tun_path):
                time.sleep(1)
            r.set(f"vpn/{name}/is_up", "no")
            result = True
            notify("[VPN]", f"`{name}` is down")
        else:
            r.set(f"vpn/{name}/is_up", "unk")
            result = False
            notify("[VPN]", f"error stopping `{name}`", urgency=URGENCY_CRITICAL, timeout=5000)
    elif meta["type"] == "ipsec":
        vpn_stop_task = subprocess.Popen(meta["down"], shell=True, env={'LANGUAGE':'en_US.en'},
                                         stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        vpn_stop_result = vpn_stop_task.wait()
        if vpn_stop_result != 0:
            vpn_stderr = vpn_stop_task.stderr.read().decode().strip().split()
            if "not an active" in " ".join(vpn_stderr):
                r.set(f"vpn/{name}/is_up", "no")
                result = True
                notify("[VPN]", f"`{name}` is down")
            else:
                r.set(f"vpn/{name}/is_up", "unk")
                result = False
                notify("[VPN]", f"error stopping `{name}`", urgency=URGENCY_CRITICAL, timeout=5000)
        else:
            r.set(f"vpn/{name}/is_up", "no")
            result = True
            notify("[VPN]", f"`{name}` is down")
    else:
        result = False
        notify("[VPN]", f"Unknown service type: {meta['type']} service", urgency=URGENCY_CRITICAL, timeout=5000)
    return result


def stop_running(omit=None):
    devdns_stop_task = subprocess.Popen("systemctl stop docker-devdns.service",
                                        shell=True, stdout=subprocess.PIPE)
    assert devdns_stop_task.wait() == 0
    if omit:
        del vpn_meta[omit]
    for vpn, meta in vpn_meta.items():
        stop_service(vpn, meta)


if args.vpn_service_tag:
    meta = vpn_meta.get(args.vpn_service_tag, None)
    if not meta:
        notify("[VPN]", f"Cannot find {args.vpn_service_tag} service", urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)
    stop_running(omit=args.vpn_service_tag)
    start_service(args.vpn_service_tag, meta)
    notify("[VPN]", f"Started {args.vpn_service_tag} service", timeout=5000)
elif args.stop_running:
    stop_running()
