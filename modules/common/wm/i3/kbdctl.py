import i3ipc
import subprocess

windows = {}
default = "us"


def get_focused_window_ws_and_class(i3):
    focused = i3.get_tree().find_focused()
    return focused.workspace().num, focused.window_class


def get_window_id(i3):
    ws, _class = get_focused_window_ws_and_class(i3)
    return f"{ws}-{_class}"


def get_current_kbd_layout():
    return subprocess.Popen("xkb-switch", shell=True,
                            stdout=subprocess.PIPE).stdout.read().strip().decode("utf-8")


def on_focus(i3, e):
    name = get_window_id(i3)
    saved_layout = windows.get(name, default)
    current_layout = get_current_kbd_layout()
    if current_layout != saved_layout:
        subprocess.Popen("xkb-switch -n", shell=True,
                         stdout=subprocess.PIPE)


def on_binding(i3, e):
    if e.binding.command == "nop" and e.binding.symbol == "backslash":
        subprocess.Popen("xkb-switch -n", shell=True,
                         stdout=subprocess.PIPE)
        windows[get_window_id(i3)] = get_current_kbd_layout()

i3 = i3ipc.Connection()

i3.on("window::focus", on_focus)
i3.on("binding::run", on_binding)

i3.main()
