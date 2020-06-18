import re
import subprocess
import sys
from urllib.request import urlopen

import dmenu
from bs4 import BeautifulSoup


@pythonPatchNotify@

def is_valid_url(url):
    return re.search("@urlRegexPy@", url) is not None

page_url_task = subprocess.Popen("xsel -o -b",
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
        notify("[scrape]", "Scraped {0} links".format(len(org_content) - 2), timeout=5000)
        for line in org_content:
            print(line)
    else:
        print(page_url)
        notify("[scrape]", "Non-URL content in clipboard, skipping", timeout=5000)
