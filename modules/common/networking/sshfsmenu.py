import argparse
import json
import subprocess
import sys

from pystdlib.uishim import get_selection, notify
import redis



opts = "-oauto_cache,reconnect,Compression=no" # for speeding things up
parser = argparse.ArgumentParser(description="Mount projects over SSHFS.")
parser.add_argument("--mode", dest="mode", choices = ["mount", "unmount"],
                    default="mount", help="script acting mode")

args = parser.parse_args()


r = redis.Redis(host='localhost', port=6379, db=0)

if args.mode == "mount":
    sshfs_map = json.loads(r.get("net/sshfs_map"))
    sshfs_mounts = r.get("net/sshfs_mounts")
    if sshfs_mounts:
        sshfs_mounts = json.loads(sshfs_mounts)
    else:
        sshfs_mounts = {}

    remote = get_selection(sshfs_map.keys(), "mount", case_insensitive=True, lines=10, font="@wmFontDmenu@")
    if not remote:
        notify("[sshfs]", "nothing selected", timeout=5000)
        sys.exit(1)
    local = sshfs_map[remote]
    ensure_local_path_task = subprocess.Popen(f"mkdir -p {local}", shell=True, stdout=subprocess.PIPE)
    result = ensure_local_path_task.wait()
    if result != 0:
        notify("[sshfs]", f"cannot create {local}", urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)

    sshfs_mount_task = subprocess.Popen(f"sshfs {remote} {local} {opts}", shell=True,
                                        stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    result = sshfs_mount_task.wait()
    if result == 0:
        sshfs_mounts[remote] = local
        r.set("net/sshfs_mounts", json.dumps(sshfs_mounts))
        notify("[sshfs]", f"mounted {remote} at {local}", timeout=5000)
    else:
        notify("[sshfs]", f"mount failed: {remote}:\ncause: {sshfs_mount_task.stderr.read().decode()}",
               urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)
elif args.mode == "unmount":
    sshfs_mounts = r.get("net/sshfs_mounts")
    if sshfs_mounts:
        sshfs_mounts = json.loads(sshfs_mounts)
    else:
        notify("[sshfs]", "nothing mounted", timeout=5000)
        sys.exit(1)

    remote = get_selection(sshfs_mounts.keys(), "unmount", case_insensitive=True, lines=10, font="@wmFontDmenu@")
    local = sshfs_mounts[remote]

    if not remote:
        notify("[sshfs]", "nothing to unmount", timeout=5000)
        sys.exit(1)
    sshfs_unmount_task = subprocess.Popen(f"fusermount -u {local}", shell=True, stdout=subprocess.PIPE)
    result = sshfs_unmount_task.wait()
    if result == 0:
        del sshfs_mounts[remote]
        r.set("net/sshfs_mounts", json.dumps(sshfs_mounts))
        notify("[sshfs]", f"unmounted {remote} from {local}", timeout=5000)
    else:
        notify("[sshfs]", f"failed unmounting {remote} from {local}", urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)
else:
    notify("[sshfs]", f"unknown mode: {args.mode}", timeout=5000)
    sys.exit(1)
