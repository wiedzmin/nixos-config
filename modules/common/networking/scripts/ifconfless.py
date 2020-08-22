from pystdlib.uishim import get_selection, show_text_dialog
from pystdlib import shell_cmd


ifconfig_result = shell_cmd("ifconfig -s", split_output="\n")
iface_names = [iface_meta.split()[0] for iface_meta in ifconfig_result[1:]]

iface_name = get_selection(iface_names, "describe", case_insensitive=True, lines=10, font="@wmFontDmenu@")
iface_traits = shell_cmd(f"ifconfig {iface_name}")

shell_cmd(["xsel", "-ib"], universal_newlines=True, input=f"{iface_traits.encode('utf-8')}")
show_text_dialog(text=iface_traits)
