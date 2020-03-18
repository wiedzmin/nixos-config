import os
import re
import subprocess
import sys
import time

import dmenu
import notify2
from notify2 import URGENCY_NORMAL, URGENCY_CRITICAL

URL_REGEX = "(https?|ftp|file)://[-A-Za-z0-9\+&@#/%?=~_|!:,.;]*[-A-Za-z0-9\+&@#/%=~_|]"


def is_valid_url(url):
    return re.search(URL_REGEX, url) is not None


def fetch_tags_cloud():
    tags_cloud_task = subprocess.Popen("buku --np --st",
                                  shell=True, stdout=subprocess.PIPE)
    result = [" ".join(tag.strip().split(" ")[1:-1])
              for tag in tags_cloud_task.stdout.read().decode().split("\n") if tag]
    assert tags_cloud_task.wait() == 0
    return result


notify2.init("buku_add")

bookmark_text_task = subprocess.Popen("xsel -o -b",
                                      shell=True, stdout=subprocess.PIPE)
bookmark_text = bookmark_text_task.stdout.read().decode().strip()
assert bookmark_text_task.wait() == 0
if bookmark_text is not None:
    if not is_valid_url(bookmark_text):
        n = notify2.Notification("error", "URL is not valid")
        n.set_urgency(URGENCY_CRITICAL)
        n.set_timeout(5000)
        n.show()
        sys.exit(1)
    result = dmenu.show([bookmark_text], prompt='bookmark')
    if not result:
        n = notify2.Notification("OK", "Aborted adding bookmark")
        n.set_urgency(URGENCY_NORMAL)
        n.set_timeout(5000)
        n.show()
        sys.exit(1)
    tags_cloud = fetch_tags_cloud()
    bookmark_tags = []
    while True:
        tag = dmenu.show(tags_cloud, prompt='add tag', lines=15)
        if tag:
           bookmark_tags.append(tag)
           tags_cloud.remove(tag)
        else:
           break
    if bookmark_tags:
        os.system("buku -a {0} {1}".format(
                  bookmark_text,
                  ",".join(bookmark_tags)))
    else:
        os.system("buku -a {0}".format(
                  bookmark_text))
    n = notify2.Notification("Success", "Bookmark added: {0} ({1})".format(
                             bookmark_text,
                             ",".join(bookmark_tags)))
    n.set_urgency(URGENCY_NORMAL)
    n.set_timeout(2000)
    n.show()
else:
    n = notify2.Notification("Error", "No text in clipboard")
    n.set_urgency(URGENCY_CRITICAL)
    n.set_timeout(2000)
    n.show()
    sys.exit(1)
