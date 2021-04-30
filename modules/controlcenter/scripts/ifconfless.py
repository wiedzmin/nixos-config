import argparse

from pystdlib.uishim import get_selection, show_text_dialog
from pystdlib import shell_cmd


parser = argparse.ArgumentParser(description="Repository overview search")
parser.add_argument('--dmenu-font', dest="dmenu_font", type=str, help="Dmenu font")
args = parser.parse_args()


ifconfig_result = shell_cmd("ifconfig -s", split_output="\n")
iface_names = [iface_meta.split()[0] for iface_meta in ifconfig_result[1:]]

iface_name = get_selection(iface_names, "describe", case_insensitive=True, lines=10, font=args.dmenu_font)
iface_traits = shell_cmd(f"ifconfig {iface_name}")

shell_cmd(["xsel", "-ib"], universal_newlines=True, input=f"{iface_traits.encode('utf-8')}")
show_text_dialog(text=iface_traits)
