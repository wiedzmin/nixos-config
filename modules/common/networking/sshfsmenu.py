import argparse
import json
import subprocess
import sys

import dmenu
import redis



opts = "-oauto_cache,reconnect,Compression=no" # for speeding things up
parser = argparse.ArgumentParser(description="Mount projects over SSHFS.")
parser.add_argument("--mode", dest="mode", choices = ["mount", "unmount"],
                    default="mount", help="script acting mode")

args = parser.parse_args()

@pythonPatchNotify@

r = redis.Redis(host='localhost', port=6379, db=0)

if args.mode == "mount":
    sshfs_map = json.loads(r.get("net/sshfs_map"))
    sshfs_mounts = r.get("net/sshfs_mounts")
    if sshfs_mounts:
        sshfs_mounts = json.loads(sshfs_mounts)
    else:
        sshfs_mounts = {}

    remote = dmenu.show(sshfs_map.keys(), prompt="mount", case_insensitive=True, lines=10)
    if not remote:
        notify("[sshfs]", "nothing selected", timeout=5000)
        sys.exit(1)
    local = sshfs_map[remote]
    ensure_local_path_task = subprocess.Popen("mkdir -p {0}".format(local),
                                              shell=True, stdout=subprocess.PIPE)
    result = ensure_local_path_task.wait()
    if result != 0:
        notify("[sshfs]", "cannot create {0}".format(local), urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)

    sshfs_mount_task = subprocess.Popen("sshfs {0} {1} {2}".format(remote, local, opts),
                                        shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    result = sshfs_mount_task.wait()
    if result == 0:
        sshfs_mounts[remote] = local
        r.set("net/sshfs_mounts", json.dumps(sshfs_mounts))
        notify("[sshfs]", "mounted {0} at {1}".format(remote, local), timeout=5000)
    else:
        notify("[sshfs]", "mount failed: {0}:\ncause: {1}".format(
            remote, sshfs_mount_task.stderr.read().decode()), urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)
elif args.mode == "unmount":
    sshfs_mounts = r.get("net/sshfs_mounts")
    if sshfs_mounts:
        sshfs_mounts = json.loads(sshfs_mounts)
    else:
        notify("[sshfs]", "nothing mounted", timeout=5000)
        sys.exit(1)

    remote = dmenu.show(sshfs_mounts.keys(), prompt="unmount", case_insensitive=True, lines=10)
    local = sshfs_mounts[remote]

    if not remote:
        notify("[sshfs]", "nothing to unmount", timeout=5000)
        sys.exit(1)
    sshfs_unmount_task = subprocess.Popen("fusermount -u {0}".format(local),
                                          shell=True, stdout=subprocess.PIPE)
    result = sshfs_unmount_task.wait()
    if result == 0:
        del sshfs_mounts[remote]
        r.set("net/sshfs_mounts", json.dumps(sshfs_mounts))
        notify("[sshfs]", "unmounted {0} from {1}".format(remote, local), timeout=5000)
    else:
        notify("[sshfs]", "failed unmounting {0} from {1}".format(remote, local),
               urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)
else:
    notify("[sshfs]", "unknown mode: {0}".format(args.mode), timeout=5000)
    sys.exit(1)
