import json

import redis

from pystdlib.uishim import notify, URGENCY_NORMAL, URGENCY_CRITICAL
from pystdlib import shell_cmd


r = redis.Redis(host='localhost', port=6379, db=0)
macs = json.loads(r.get("networking/wireless/headsets"))
notify(f"[BT battery]", "Please wait...", urgency=URGENCY_NORMAL, timeout=5000)
result = []
for mac in sorted(macs):
    output = shell_cmd(f"bluetooth_battery {mac}").strip()
    if "offline" in output:
        result.append(f"{mac}: offline")
    else:
        result.append(f"{mac}: {output.split(' ')[-1]}")

if result:
    notify(f"[BT battery]", "\n".join(result), urgency=URGENCY_NORMAL, timeout=3000)
else:
    notify(f"[BT battery]", "error getting headset(s) battery level(s)", urgency=URGENCY_CRITICAL, timeout=3000)
