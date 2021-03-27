import argparse
import re
import sys
from urllib.request import urlopen

from bs4 import BeautifulSoup

from pystdlib.uishim import get_selection, notify
from pystdlib.xlib import get_active_window_traits
from pystdlib import shell_cmd


def extract_url(str):
    result = re.search("@urlRegexPy@", str)
    if result:
        return result.group()
    return None


def acquire_url():
    clipboard_contents = shell_cmd("xsel -o -b")
    print(f"clipboard_contents: {clipboard_contents}")
    url = extract_url(clipboard_contents)
    print(f"url: {url}")
    if url:
        return url
    else:
        notify("[scrape]", "Non-URL content in clipboard, skipping", timeout=5000)
        win_name, win_class = get_active_window_traits()
        print(f"win_name: {win_name} | win_class: {win_class}")
        if win_name:
            url = extract_url(win_name)
            print(f"clipboard_contents: {clipboard_contents}")
            if not url:
                return None
            return url
        else:
            return None


parser = argparse.ArgumentParser(description="Collect links under chosen URL.")
parser.add_argument("--path", "-p", dest="collection_path",
                    help="Collected links save path")
args = parser.parse_args()


page_url = acquire_url()
if page_url is not None:
    session_name = get_selection([], "save as", lines=1, font="@wmFontDmenu@")
    if not session_name:
        sys.exit(1)

    page_content = urlopen(page_url)
    soup = BeautifulSoup(page_content, "html.parser")
    tags = soup.findAll("a", attrs={"href": re.compile("^https?://")})
    org_content = [f"#+TITLE: {soup.title.string}\n", f"#+PROPERTY: url {page_url}\n"]
    for tag in tags:
        org_content.append(f"* {tag.get('href')}\n")
    if args.collection_path:
        save_path = f'{args.collection_path + "/"}{session_name}.org'
    else:
        save_path = f'{session_name}.org'
    with open(save_path, "w") as f:
        f.writelines(org_content)
    notify("[scrape]", f"Scraped {len(org_content) - 2} links", timeout=5000)
    for line in org_content:
        print(line)
