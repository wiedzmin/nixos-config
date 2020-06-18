import os

import notify2
from notify2 import URGENCY_NORMAL, URGENCY_CRITICAL


notify2.init(os.path.basename(__file__))


def notify(header, msg, urgency=URGENCY_NORMAL, timeout=3000):
    n = notify2.Notification(header, msg, )
    n.set_urgency(urgency)
    n.set_timeout(timeout)
    n.show()
