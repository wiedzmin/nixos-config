from datetime import date
import argparse
import json
import sys

import redis
import requests

from pystdlib.uishim import notify, URGENCY_NORMAL, URGENCY_CRITICAL
from pystdlib import shell_cmd


parser = argparse.ArgumentParser(description="Factory calendar checks")
parser_update.add_argument('--key', dest="cache_key_name", type=str, default='scheduling/fcalendar',
                           help="Cache key name for calendar contents")
subparsers = parser.add_subparsers(help="command", dest="cmd")
parser_update = subparsers.add_parser("update", help="Updating factory calendar")
parser_update.add_argument('--year', dest="update_year", type=str, default='current',
                           choices=["current", "next"],
                           help="Use either current on next year's calendar")
parser_check = subparsers.add_parser("check", help="Check current date")
parser_check.add_argument('--dry-run', dest="check_dry_run", action="store_true",
                           default=False, help="Dry run")
parser_check.add_argument('--cmd', dest="check_cmd", type=str, help="Wrap and check this command")
args = parser.parse_args()


def get_calendar_json(next_year=False):
    year = date.today().year
    if next_year:
        year += 1
    year_str = str(year)
    url = f"http://xmlcalendar.ru/data/ru/{year_str}/calendar.json"
    r = requests.get(url, allow_redirects=True)
    return json.loads(r.content)


def prepare_calendar_json(data, dump=False):
    prepared = data
    months_data = data["months"]
    for month in months_data:
        days_str = month["days"]
        days_list = days_str.split(",")
        days_result = []
        for day in days_list:
            if not day.endswith("*"):
                days_result.append(int(day))
        month["days"] = days_result
    prepared["months"] = months_data
    result = prepared
    if dump:
        result = json.dumps(prepared)
    return result


def update_calendar_cache(next_year=False):
    r = redis.Redis(host='localhost', port=6379, db=0)
    r.set(args.cache_key_name,
          prepare_calendar_json(get_calendar_json(next_year=next_year),
                                dump=True))

def is_today_holiday():
    # TODO: assert calendar is for intended year
    r = redis.Redis(host='localhost', port=6379, db=0)
    if not r.exists(args.cache_key_name):
        notify(f"[scheduling]", "no holidays data, update calendar beforehand", urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)
    fcalendar = json.loads(r.get(args.cache_key_name))
    today = date.today()
    if fcalendar["year"] != today.year:
        notify(f"[scheduling]", "wrong calendar year, update calendar correctly", urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)
    is_holiday = False
    for month in fcalendar["months"]:
        if today.month == month["month"]:
            if today.day in month["days"]:
                is_holiday = True
                break
    return is_holiday


if args.cmd == "update":
    update_calendar_cache(next_year=(args.update_year != "current"))
if args.cmd == "check":
    if args.check_dry_run:
        is_holiday = is_today_holiday()
        notify(f"[scheduling]", f"Today is {'holiday' if is_holiday else 'not holiday'}",
               urgency=URGENCY_NORMAL, timeout=5000)
        sys.exit(0)
    if not args.check_cmd:
        notify(f"[scheduling]", "no command to wrap", urgency=URGENCY_CRITICAL, timeout=5000)
        sys.exit(1)
    if not is_today_holiday():
        shell_cmd(args.check_cmd, oneshot=True)
