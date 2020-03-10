import re
import subprocess
import sys
from urllib.request import urlopen

import dmenu
import notify2
from notify2 import URGENCY_NORMAL
from bs4 import BeautifulSoup


def is_valid_url(url):
    return re.search("@urlRegex@", url) is not None

notify2.init("collect_links_on_page")

page_url_task = subprocess.Popen("@xselBinary@ -o -b",
                                 shell=True, stdout=subprocess.PIPE)
page_url = page_url_task.stdout.read().decode().strip()
assert page_url_task.wait() == 0
if page_url is not None:
    if is_valid_url(page_url):
        session_name = dmenu.show([], prompt="save as",
                                  case_insensitive=True, lines=1)
        if not session_name:
            sys.exit(1)

        page_content = urlopen(page_url)
        soup = BeautifulSoup(page_content, "html.parser")
        tags = soup.findAll("a", attrs={"href": re.compile("^https?://")})
        org_content = [
            "#+TITLE: {0}\n".format(soup.title.string),
            "#+PROPERTY: url {0}\n".format(page_url)
        ]
        for tag in tags:
            org_content.append("* {0}\n".format(tag.get('href')))
        with open("@firefoxSessionsPath@/{0}.org".format(session_name), "w") as f:
            f.writelines(org_content)
        n = notify2.Notification("[scrape]", "Scraped {0} links".format(len(org_content) - 2))
        n.set_urgency(URGENCY_NORMAL)
        n.set_timeout(5000)
        n.show()
        for line in org_content:
            print(line)
    else:
        print(page_url)
        n = notify2.Notification("[scrape]", "Non-URL content in clipboard, skipping")
        n.set_urgency(URGENCY_NORMAL)
        n.set_timeout(5000)
        n.show()
