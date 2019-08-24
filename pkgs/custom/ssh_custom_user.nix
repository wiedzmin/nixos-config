{ bash, eternal-terminal, gawk, rofi, tmux, ... }:
''
  #!${bash}/bin/bash

  function show_list() {
      contents=("$@")
      for i in "''${contents[@]}";
      do
          echo "$i"
      done
  }

  # TODO: provide freeform option or predefined list on Nix level
  USERS=(
    "root"
    "alex3rd" # FIXME: parameterize back again
    "octocat"
  )

  main() {
      USER=$( (show_list "''${USERS[@]}") | ${rofi}/bin/rofi -dmenu -p "User" )
      if [ ! -n "$USER" ]; then
          exit 1
      fi
      HOST=$( cat /etc/hosts | ${gawk}/bin/awk '{print $2}' | ${rofi}/bin/rofi -dmenu -p "Host" )
      if [ -n "$HOST" ]; then
          ${tmux}/bin/tmux new-window "${eternal-terminal}/bin/et $USER@$HOST"
      fi
  }

  main

  exit 0
''
