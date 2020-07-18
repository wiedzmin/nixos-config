import os
import sys

from pyfzf.pyfzf import FzfPrompt
import dmenu


is_interactive = sys.stdin.isatty()
in_xsession = os.environ.get("DISPLAY")


def get_selection(seq, prompt, lines=5):
    if in_xsession:
        return dmenu.show(seq, prompt=prompt, lines=lines)
    else:
        fzf = FzfPrompt()
        return fzf.prompt(seq, '--cycle')[0]


def log():
    pass

# notify(header, msg, urgency=URGENCY_NORMAL, timeout=3000)
