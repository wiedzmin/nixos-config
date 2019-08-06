{ bash, config, lib, pkgs, rofi, ... }:
with import ../../../pkgs/util.nix {inherit config lib pkgs;};
with import ../secrets/const.nix {inherit lib config pkgs;};
''
    #!${bash}/bin/bash

    function show_mapping_keys() {
        eval "declare -A contents="''${1#*=}
        for i in "''${!contents[@]}";
        do
            echo "$i"
        done
    }

    ${listOfSetsToShellHashtable (jobWebjumps ++ webjumps) "url" "WEBJUMPS" true}

    main() {
        WEBJUMP=$( (show_mapping_keys "$(declare -p WEBJUMPS)") | ${rofi}/bin/rofi -dmenu -p "Jump to" )
        if [ -n "$WEBJUMP" ]; then
            ''${WEBJUMPS[$WEBJUMP]} "$WEBJUMP"
        fi
    }

    main

    exit 0
''
