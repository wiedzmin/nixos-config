import argparse
import pathlib
import re
import shutil
import sys

from os import listdir
from os.path import isfile, join

from pystdlib.uishim import notify, URGENCY_CRITICAL


DATE_REGEXPS = [
    "screenshot-(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})_[0-9]{2}:[0-9]{2}:[0-9]{2}",
    "screenshot-(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})_[0-9]{2}-[0-9]{2}-[0-9]{2}",
    "(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})_[0-9]{2}:[0-9]{2}:[0-9]{2}_[0-9]+x[0-9]+_scrot",
    "(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})-[0-9]{6}_[0-9]+x[0-9]+_scrot",
    "screenshot-(?P<day>[0-9]{2})-(?P<month>[0-9]{2})-(?P<year>[0-9]{4})-[0-9]{2}:[0-9]{2}:[0-9]{2}",
    "screenshot-[0-9]{2}:[0-9]{2}:[0-9]{2} (?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})",
    "screenshot-[0-9]{2}:[0-9]{2}:[0-9]{2}_(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})",
]


parser = argparse.ArgumentParser(description="Screenshots ordering")
parser.add_argument('--base-dir', dest="base_dir", type=str, help="Base directory to perform sorting under")
parser.add_argument('--fallback-dir', dest="fallback_dir", default="named", type=str,
                    help="Directory under base to place named/non-matched screenshots to")
args = parser.parse_args()

if not args.base_dir:
    print("No base directory provided")
    sys.exit(1)

files = [f for f in listdir(args.base_dir) if isfile(join(args.base_dir, f))]
if not files:
    notify("[screenshots]", f"No screenshots found", urgency=URGENCY_CRITICAL, timeout=5000)
else:
    for f in files:
        fail_count = 0
        for regexp in DATE_REGEXPS:
            m = re.match(regexp, f)
            if m:
                year = m.group("year")
                month = m.group("month")
                day = m.group("day")
                pathlib.Path(f"{args.base_dir}/{year}/{month}/{day}").mkdir(parents=True, exist_ok=True)
                shutil.move(f"{args.base_dir}/{f}", f"{args.base_dir}/{year}/{month}/{day}")
                notify("[screenshots]", f"{f} --> {args.base_dir}/{year}/{month}/{day}", timeout=5000)
                break
            else:
                fail_count += 1
        if fail_count == len(DATE_REGEXPS):
            notify("[screenshots]", f"`{f}` did not matched any regexps, custom name encountered",
                   urgency=URGENCY_CRITICAL, timeout=5000)
            pathlib.Path(f"{args.base_dir}/{args.fallback_dir}").mkdir(parents=True, exist_ok=True)
            shutil.move(f"{args.base_dir}/{f}", f"{args.base_dir}/{args.fallback_dir}")
