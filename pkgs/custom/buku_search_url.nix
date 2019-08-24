{ bash, buku, rofi, ... }:
''
  #!${bash}/bin/bash

  _rofi () {
      ${rofi}/bin/rofi -dmenu -i -no-levenshtein-sort -width 1000 "$@"
  }

  main() {
      SEARCH_RESULTS="$(${buku}/bin/buku -f 1 --nc -p)"
      SELECTION=$( echo "$SEARCH_RESULTS" | tr ' ' '\n' | _rofi -p '> ')
      if [ -n "$SELECTION" ]; then
          ${buku}/bin/buku -o $SELECTION
      fi
  }

  main

  exit 0
''
