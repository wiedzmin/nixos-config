{ bash, fd, rofi, zathura, ... }:
let
    bookshelfPath = "$HOME/bookshelf"; # TODO: parameterize at nix level
in
''
    #!${bash}/bin/bash

    IFS=$'\n'
    BOOKS=$(${fd}/bin/fd --full-path ${bookshelfPath} -e pdf -e djvu)

    function show_list() {
        contents=("$@")
        for i in "''${contents[@]}";
        do
            echo "$i"
        done
    }

    main() {
        SELECTED_BOOK=$( (show_list "''${BOOKS[@]}") | ${rofi}/bin/rofi -dmenu -p "EBook " )
        if [ -n "$SELECTED_BOOK" ]; then
            ${zathura}/bin/zathura "$SELECTED_BOOK" & >& /dev/null
        fi
    }

    main

    exit 0
''
