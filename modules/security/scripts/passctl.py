import argparse

from pystdlib.uishim import get_selection
from pystdlib.passutils import collect_entries, read_entry_raw, annotate_entry
from pystdlib import shell_cmd


parser = argparse.ArgumentParser(description="Some pass automation")
parser.add_argument('--store', dest="store_path",
                    default=f"{os.environ['HOME']}/.password-store",
                    type=str, help="Passwords store path")
parser.add_argument("--add", dest="add_entry", action="store_true",
                    default=False, help="Add new pass entry")
parser.add_argument('--dmenu-font', dest="dmenu_font", type=str, help="Dmenu font")
args = parser.parse_args()

# provide copying to clipboard
# provide option to use (py)fzf
if args.add_entry:
    # how to:
    # iterate over nested levels, collecting nodes under previously selected nodes
    # full entry path should be accumulated during this loop
    # on every level we check if current input exists as path part
    # if it exists we are going deeper
    # otherwise we create inexistent node and starting the loop over
    # there should be a show-stopper keybinding to be able to end this loop
    # afterwards we get last component of accumelated path and assuming it to be
    # leaf(gpg) node, that will actually contain secret data

    # then ask password type - manual or autogenerated
    # think of how to provide password length in autogenerated case though
    print("add entry")
else:
    pass_files = collect_entries(args.store_path)
    path = get_selection(pass_files, ">", lines=10, font=args.dmenu_font)

    if path:
        annotated = annotate_entry(read_entry_raw(path))
        field = get_selection(annotated.keys(), "type >", lines=3, font=args.dmenu_font)
        if field:
            shell_cmd(f"xdotool type {annotated[field]}")
