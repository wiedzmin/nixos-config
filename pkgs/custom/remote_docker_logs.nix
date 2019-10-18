{ bash, config, dunst, eternal-terminal, lib, openssh, pkgs, rofi, systemd, tmux, ... }:
# TODO: think of decoupling from job infra
''
  #!${bash}/bin/bash

  ${config.secrets.job.enforceJobVpnHunkSh}

  ask_for_logs() {
      LOGS=$(${openssh}/bin/ssh ${config.secrets.job.infra.logsHost} "find ${config.secrets.job.infra.remoteDockerLogsRoot}/ -maxdepth 1 -size +0 -type f | grep -v gz")
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
          ${config.secrets.job.infra.logsHost} \
          -c 'tail -f $LOG'"
      fi
  }

  main

  exit 0
''
