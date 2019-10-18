{ bash, config, gawk, lib, pkgs, rofi, xsel, ... }:
''
  #!${bash}/bin/bash

  ${config.secrets.nav.searchenginesData}

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
          # IDEA: extract browsers module
          firefox --new-window "$URL"
      fi
  }

  main

  exit 0
''
