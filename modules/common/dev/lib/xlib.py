import Xlib.protocol.event
from Xlib import X, display, error, Xatom, Xutil


def sendEvent(win, ctype, data, mask=None):
    data = (data+[0]*(5-len(data)))[:5]
    ev = Xlib.protocol.event.ClientMessage(window=win, client_type=ctype, data=(32,(data)))

    if not mask:
        mask = (X.SubstructureRedirectMask|X.SubstructureNotifyMask)
    win.send_event(ev, event_mask=mask)


def switch_desktop(index):
    display = Xlib.display.Display()
    sendEvent(display.screen().root, display.intern_atom("_NET_CURRENT_DESKTOP"),
              [index, X.CurrentTime])
    display.flush()
