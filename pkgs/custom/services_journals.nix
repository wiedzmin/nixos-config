{ bash, gawk, rofi, systemd, tmux, ... }: ''
  #!${bash}/bin/bash

  function show_list() {
      contents=("$@")
      for i in "''${contents[@]}";
      do
          echo "$i"
      done
  }

  SERVICE_CONTEXTS=(
    "system"
    "user"
  )

  main() {
      CONTEXT=$( (show_list "''${SERVICE_CONTEXTS[@]}") | ${rofi}/bin/rofi -dmenu -p "Context" )
      if [ ! -n "$CONTEXT" ]; then
          exit 1
      fi
      SERVICE=$(${systemd}/bin/systemctl $([[ "$CONTEXT" == "user" ]] && echo --user) list-unit-files | \
                grep -v target | ${gawk}/bin/awk '{print $1}' | \
                ${rofi}/bin/rofi -dmenu -p "Service")
      if [ -n "$SERVICE" ]; then
          ${tmux}/bin/tmux new-window "${systemd}/bin/journalctl $([[ "$CONTEXT" == "user" ]] && echo --user) -u $SERVICE"
      fi
  }

  main

  exit 0
''
