{ bash, config, gawk, lib, pkgs, rofi, xsel, ... }:
with import ../../pkgs/util.nix {inherit config lib pkgs;};
''
    #!${bash}/bin/bash

    ${listOfSetsToShellHashtable (config.misc.searchEngines) "engine" "SEARCHENGINES" true}

    list_searchengines() {
        INDEX=1
        for i in "''${!SEARCHENGINES[@]}"
        do
            echo "$INDEX $i"
            (( INDEX++ ))
        done
    }

    main() {
        SELECTED_ENGINE=$( (list_searchengines) | ${rofi}/bin/rofi -dmenu -i -p "Search" | ${gawk}/bin/awk '{print $2}')
        if [ ! -n "$SELECTED_ENGINE" ]; then
            exit 1
        fi
        QUERY=$(${xsel}/bin/xsel -o)
        if [ -n "$QUERY" ]; then
            URL="''${SEARCHENGINES[$SELECTED_ENGINE]}$QUERY"
            ${config.misc.defaultBrowserCmd} "$URL"
        fi
    }

    main

    exit 0
''
