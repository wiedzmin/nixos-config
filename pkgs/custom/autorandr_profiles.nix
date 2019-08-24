{ autorandr, bash, fd, gnugrep, rofi, ... }:
''
  #!${bash}/bin/bash

  AUTORANDR_PROFILES_PATH=''${1:-$HOME/.config/autorandr}

  AUTORANDR_PROFILES=(
  $(${fd}/bin/fd --type d . $AUTORANDR_PROFILES_PATH -x echo '{/}' | ${gnugrep}/bin/grep -ve "\.d")
  )

  function show_list() {
      contents=("$@")
      for i in "''${contents[@]}";
      do
          echo "$i"
      done
  }

  main() {
      SELECTED_PROFILE=$( (show_list "''${AUTORANDR_PROFILES[@]}") | ${rofi}/bin/rofi -dmenu -p "Profile " )
      if [ -n "$SELECTED_PROFILE" ]; then
          ${autorandr}/bin/autorandr --load "$SELECTED_PROFILE" & >& /dev/null
      fi
  }

  main

  exit 0
''
