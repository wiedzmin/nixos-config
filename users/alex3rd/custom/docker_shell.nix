{ bash, config, coreutils, docker, docker-machine, dunst, eternal-terminal, gawk, lib, pkgs, rofi, systemd, tmux, ... }:
with import ../secrets/const.nix { inherit lib config pkgs; };
let
  dockerContainerShellExecutable = "/bin/bash";
in
''
  #!${bash}/bin/bash

  ${enforce_job_vpn_impl}

  main() {
      HOST=$( cat /etc/hosts | ${gawk}/bin/awk '{print $2}' | ${coreutils}/bin/uniq | ${rofi}/bin/rofi -dmenu -p "Host" )
      if [ -n $HOST ]; then
          if [ "$HOST" == "localhost" ]; then
              eval $(${docker-machine}/bin/docker-machine env -u)
          else
              enforce_job_vpn
              eval $(${docker-machine}/bin/docker-machine env $HOST)
          fi
          SELECTED_CONTAINER=$( ${docker}/bin/docker ps --format '{{.Names}}' | ${rofi}/bin/rofi -dmenu -p "Container" )
          if [ -n "$SELECTED_CONTAINER" ]; then
              ${tmux}/bin/tmux new-window "${eternal-terminal}/bin/et \
              $HOST \
              -c 'docker exec -it $SELECTED_CONTAINER ${dockerContainerShellExecutable}'"
          fi
      fi
  }

  main

  exit 0
''
