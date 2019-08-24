{ bash, config, dunst, eternal-terminal, lib, openssh, pkgs, rofi, systemd, tmux, ... }:
# TODO: think of decoupling from job infra
with import ../secrets/const.nix { inherit lib config pkgs; };
''
  #!${bash}/bin/bash

  ${enforce_job_vpn_impl}

  ask_for_logs() {
      LOGS=$(${openssh}/bin/ssh ${jobInfraLogsHost} "find ${jobInfraRemoteDockerLogsPath}/ -maxdepth 1 -size +0 -type f | grep -v gz")
      for i in "''${LOGS[@]}"
      do
          echo "$i"
      done
  }

  main() {
      LOG=$( (ask_for_logs) | ${rofi}/bin/rofi -dmenu -p "View log" )
      if [ -n "$LOG" ]; then
          enforce_job_vpn
          ${tmux}/bin/tmux new-window "${eternal-terminal}/bin/et \
          ${jobInfraLogsHost} \
          -c 'tail -f $LOG'"
      fi
  }

  main

  exit 0
''
