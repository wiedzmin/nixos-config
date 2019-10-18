{ bash, config, lib, pkgs, rofi, ... }:
''
  #!${bash}/bin/bash

  function show_mapping_keys() {
      eval "declare -A contents="''${1#*=}
      for i in "''${!contents[@]}";
      do
          echo "$i"
      done
  }

  ${config.secrets.nav.webjumpsData}

  main() {
      WEBJUMP=$( (show_mapping_keys "$(declare -p WEBJUMPS)") | ${rofi}/bin/rofi -dmenu -p "Jump to" )
      if [ -n "$WEBJUMP" ]; then
          ''${WEBJUMPS[$WEBJUMP]} "$WEBJUMP"
      fi
  }

  main

  exit 0
''
