{ bash, config, dunst, eternal-terminal, gawk, lib, openssh, pkgs, rofi, systemd, tmux, yad, ... }:
# TODO: think of decoupling from job infra
with import ../secrets/const.nix { inherit lib config pkgs; };
let
  dockerStackPsCustomFormat = "{{.Name}}   {{.Image}}   {{.Node}} {{.DesiredState}}   {{.CurrentState}}";
  useDockerStackPsCustomFormat = false;
  dockerStackShowOnlyRunning = true;
in
''
  #!${bash}/bin/bash

  ${enforce_job_vpn_impl}

  enforce_job_vpn

  SWARM_NODES=(
  ${builtins.concatStringsSep "\n"
  (
    map (host: builtins.head host.hostNames)
      (
        builtins.filter (host: host.swarm == true)
          jobExtraHosts
      )
  )}
  )
  SWARM_LEADER_NODE=$(${openssh}/bin/ssh ${jobInfraSeedHost} "docker node ls --format '{{.Hostname}} {{ .ManagerStatus }}' | grep Leader | cut -f1 -d\ ")

  docker_stack_ps_params() {
      echo ${ if dockerStackShowOnlyRunning then "--filter \\\"desired-state=Running\\\"" else ""}
           ${ if useDockerStackPsCustomFormat then " --format \\\"${dockerStackPsCustomFormat}\\\""
else "" }
  }

  MODES=(
    "status"
    "logs"
  )

  ask_for_mode() {
      for i in "''${MODES[@]}"
      do
          echo "$i"
      done
  }

  ask_for_stack() {
      STACKS=$(${openssh}/bin/ssh $SWARM_LEADER_NODE \
                                       "docker stack ls | awk '{if(NR>1)print $1}'" | \
                                       ${gawk}/bin/awk '{print $1}')
      for i in "''${STACKS[@]}"
      do
          echo "$i"
      done
  }

  show_stack_status() {
      STACK=$1
      ${openssh}/bin/ssh $SWARM_LEADER_NODE \
      "docker stack ps $STACK $(docker_stack_ps_params)" > /tmp/docker_stack_status
      ${yad}/bin/yad --filename /tmp/docker_stack_status --text-info
      rm /tmp/docker_stack_status
  }

  ask_for_stack_task() {
      STACK=$1
      TASKS=$(${openssh}/bin/ssh $SWARM_LEADER_NODE \
      "docker stack ps $STACK $(docker_stack_ps_params)" | awk '{if(NR>1)print $0}')
      SERVICE=$(${openssh}/bin/ssh $SWARM_LEADER_NODE \
      "docker service ls --format='{{.Name}}' | grep $STACK ")
      TASKS="''${SERVICE}
  ''${TASKS}"
      for i in "''${TASKS[@]}"
      do
          echo "$i"
      done
  }

  main() {
      MODE=$( (ask_for_mode) | ${rofi}/bin/rofi -dmenu -p "Mode" )
      STACK=$( (ask_for_stack) | ${rofi}/bin/rofi -dmenu -p "Stack" )
      case "$MODE" in
          status)
              show_stack_status $STACK
              ;;
          logs)
              TASK=$( (ask_for_stack_task $STACK) | ${rofi}/bin/rofi -dmenu -p "Task" | ${gawk}/bin/awk '{print $1}' )
              ${tmux}/bin/tmux new-window "${eternal-terminal}/bin/et \
                                                $SWARM_LEADER_NODE \
                                                -c 'docker service logs --follow $TASK'"
              ;;
          *)
              echo "Unknown mode: $MODE"
              exit 1
              ;;
      esac
  }

  main

  exit 0
''
