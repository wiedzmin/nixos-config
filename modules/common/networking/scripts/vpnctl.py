import argparse
import json
import subprocess
import sys
import time
import os

import redis
import dbus

from pystdlib.uishim import notify

IPV4_STATUS_PATH = "/proc/sys/net/ipv4/conf/"
NM_VPN_ACTIVE_STATUS_CODES = ["3", "5"]

parser = argparse.ArgumentParser(description="Manage enabled VPN services.")
parser.add_argument("--start", dest="vpn_service_tag", help="Start selected VPN service")
parser.add_argument("--stop-running", dest="stop_running", action="store_true",
                   default=False, help="Stop currently running VPN service")
parser.add_argument("--status", dest="vpn_status", action="store_true",
                   default=False, help="Get statuses of all touched VPN services")
parser.add_argument("--verbose", dest="verbose", action="store_true",
                   default=False, help="Be verbose")


args = parser.parse_args()


r = redis.Redis(host='localhost', port=6379, db=0)
vpn_meta = json.loads(r.get("net/vpn_meta"))

# TODO: adapt `shell_cmd` to this script specifics

def is_nm_vpn_up(name):
    vpn_status_task = subprocess.Popen(f"@nmcliBinary@ con show id {name}", shell=True,
                                       env={'LANGUAGE':'en_US.en'}, stdout=subprocess.PIPE)
    grep_vpn_state_task = subprocess.Popen(f"grep VPN.VPN-STATE", shell=True,
                                           stdin=vpn_status_task.stdout, stdout=subprocess.PIPE)
    result = grep_vpn_state_task.stdout
    return result and any([code in result for code in NM_VPN_ACTIVE_STATUS_CODES])


def start_service(name, meta):
    result = False
    if meta["type"] == "ovpn":
        tun_path = IPV4_STATUS_PATH + meta["dev"]
        if os.path.exists(tun_path):
            r.set(f"vpn/{name}/is_up", "yes")
            result = True
            if args.verbose:
                notify("[VPN]", f"`{name}` is up")
        else:
            vpn_start_task = subprocess.Popen(meta["up"], shell=True, stdout=subprocess.PIPE)
            if vpn_start_task.wait() == 0:
                while not os.path.exists(tun_path):
                    time.sleep(1)
                r.set(f"vpn/{name}/is_up", "yes")
                result = True
                notify("[VPN]", f"Started {args.vpn_service_tag} service", timeout=5000)
            else:
                r.set(f"vpn/{name}/is_up", "unk")
                result = False
                notify("[VPN]", f"error starting `{name}`\n\n{vpn_start_task.stdout.read().decode()}",
                       urgency=URGENCY_CRITICAL, timeout=5000)
    elif meta["type"] == "ipsec":
        if is_nm_vpn_up(name):
            r.set(f"vpn/{name}/is_up", "yes")
            result = True
            if args.verbose:
                notify("[VPN]", f"`{name}` is up")
        else:
            vpn_start_task = subprocess.Popen(meta["up"], shell=True, env={'LANGUAGE':'en_US.en'},
                                              stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            vpn_start_result = vpn_start_task.wait()
            if vpn_start_result != 0:
                vpn_stderr = vpn_start_task.stderr.read().decode().strip().split()
                if "is already active" in " ".join(vpn_stderr):
                    r.set(f"vpn/{name}/is_up", "yes")
                    result = True
                    if args.verbose:
                        notify("[VPN]", f"`{name}` is up")
                else:
                    r.set(f"vpn/{name}/is_up", "unk")
                    result = False
                    notify("[VPN]", f"error starting `{name}`\n\n{' '.join(vpn_stderr)}",
                           urgency=URGENCY_CRITICAL, timeout=5000)
            else:
                r.set(f"vpn/{name}/is_up", "yes")
                result = True
                notify("[VPN]", f"Started {args.vpn_service_tag} service", timeout=5000)
    else:
        result = False
        notify("[VPN]", f"Unknown service type of '{name}': {meta['type']}",
               urgency=URGENCY_CRITICAL, timeout=5000)
    return result


def stop_service(name, meta):
    result = False
    if meta["type"] == "ovpn":
        tun_path = IPV4_STATUS_PATH + meta["dev"]
        if not os.path.exists(tun_path):
            r.set(f"vpn/{name}/is_up", "no")
            result = True
            if args.verbose:
                notify("[VPN]", f"`{name}` is down")
        else:
            vpn_stop_task = subprocess.Popen(meta["down"], shell=True, stdout=subprocess.PIPE)
            if vpn_stop_task.wait() == 0:
                while os.path.exists(tun_path):
                    time.sleep(1)
                r.set(f"vpn/{name}/is_up", "no")
                result = True
                if args.verbose:
                    notify("[VPN]", f"`{name}` is down")
            else:
                r.set(f"vpn/{name}/is_up", "unk")
                result = False
                notify("[VPN]", f"error stopping `{name}`\n\n{vpn_stop_task.stdout.read().decode()}",
                       urgency=URGENCY_CRITICAL, timeout=5000)
    elif meta["type"] == "ipsec":
        if not is_nm_vpn_up(name):
            r.set(f"vpn/{name}/is_up", "no")
            result = True
            if args.verbose:
                notify("[VPN]", f"`{name}` is down")
        else:
            vpn_stop_task = subprocess.Popen(meta["down"], shell=True, env={'LANGUAGE':'en_US.en'},
                                             stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            vpn_stop_result = vpn_stop_task.wait()
            if vpn_stop_result != 0:
                vpn_stderr = vpn_stop_task.stderr.read().decode().strip().split()
                if "not an active" in " ".join(vpn_stderr):
                    r.set(f"vpn/{name}/is_up", "no")
                    result = True
                    if args.verbose:
                        notify("[VPN]", f"`{name}` is down")
                else:
                    r.set(f"vpn/{name}/is_up", "unk")
                    result = False
                    notify("[VPN]", f"error stopping `{name}`\n\n{' '.join(vpn_stderr)}",
                           urgency=URGENCY_CRITICAL, timeout=5000)
            else:
                r.set(f"vpn/{name}/is_up", "no")
                result = True
                if args.verbose:
                    notify("[VPN]", f"`{name}` is down")
    else:
        result = False
        notify("[VPN]", f"Unknown service type of '{name}': {meta['type']}",
               urgency=URGENCY_CRITICAL, timeout=5000)
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
    if r.get(f"vpn/{args.vpn_service_tag}/is_up") != "yes":
        stop_running(omit=args.vpn_service_tag)
        start_service(args.vpn_service_tag, meta)
elif args.stop_running:
    stop_running()
elif args.vpn_status:
    statuses = {}
    for key in r.scan_iter("vpn/*/is_up"):
        value = r.get(key)
        statuses[key.decode().split("/")[1]] = value.decode()
    notify("[VPN]", "\n".join([f"{key}: {value}" for key, value in statuses.items()]),
           urgency=URGENCY_NORMAL, timeout=5000)
