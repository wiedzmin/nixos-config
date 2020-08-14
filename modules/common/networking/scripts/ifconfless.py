import os
import subprocess

from pystdlib.uishim import get_selection


get_ifaces_task = subprocess.Popen("ifconfig -s", shell=True, stdout=subprocess.PIPE)
iface_names = [iface_meta.split()[0] for iface_meta in get_ifaces_task.stdout.read().decode().strip().split("\n")[1:]]

iface_name = get_selection(iface_names, "describe", case_insensitive=True, lines=10, font="@wmFontDmenu@")

get_iface_traits_task = subprocess.Popen(f"ifconfig {iface_name}", shell=True, stdout=subprocess.PIPE)
iface_traits = get_iface_traits_task.stdout.read().decode()

fill_clipboard_task = subprocess.Popen("xsel -ib", shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
fill_clipboard_task.stdin.write(iface_traits.encode("utf-8"))
with open("/tmp/iface_traits", "w") as f:
    f.write(iface_traits)

show_dialog_task = subprocess.Popen("yad --filename /tmp/iface_traits --text-info",
                                    shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
show_dialog_task.wait()
os.remove("/tmp/iface_traits")
