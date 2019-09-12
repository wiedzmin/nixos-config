{ bash, fd, rofi, tmuxp, ... }: ''
  #!${bash}/bin/bash

  function show_list() {
      contents=("$@")
      for i in "''${contents[@]}";
      do
          echo "$i"
      done
  }

  TMUXP_SESSIONS_PATH=''${1:-$HOME/tmuxp}

  TMUXP_SESSIONS=(
  $(${fd}/bin/fd --maxdepth 1 --type l '.yml' $TMUXP_SESSIONS_PATH -x echo '{/.}')
  )

  main() {
      SELECTED_SESSION=$( (show_list "''${TMUXP_SESSIONS[@]}") | ${rofi}/bin/rofi -dmenu -p "Profile " )
      if [ -n "$SELECTED_SESSION" ]; then
          ${tmuxp}/bin/tmuxp load -y -d $TMUXP_SESSIONS_PATH/$SELECTED_SESSION.yml >/dev/null 2>&1 &
      fi
  }

  main

  exit 0
''
