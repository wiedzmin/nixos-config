{ bash, config, dunst, eternal-terminal, gawk, lib, pkgs, rofi, systemd, tmux, ... }:
# TODO: think of decoupling from job infra
with import ../secrets/const.nix { inherit lib config pkgs; }; ''
  #!${bash}/bin/bash

  ${enforce_job_vpn_impl}

  main() {
      HOST=$( cat /etc/hosts | ${gawk}/bin/awk '{print $2}' | ${rofi}/bin/rofi -dmenu -p "Host" )
      if [ -n "$HOST" ]; then
          enforce_job_vpn
          ${tmux}/bin/tmux new-window "${eternal-terminal}/bin/et \
          $HOST -c 'ctop'"
      fi
  }

  main

  exit 0
''
