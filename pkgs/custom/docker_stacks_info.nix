{ bash, config, dunst, eternal-terminal, gawk, openssh, rofi, systemd, tmux, yad, ... }: # TODO: think of decoupling from job infra
let
    dockerStackPsCustomFormat = "{{.Name}}   {{.Image}}   {{.Node}} {{.DesiredState}}   {{.CurrentState}}";
    useDockerStackPsCustomFormat = false;
    dockerStackShowOnlyRunning = true;
in
''
    #!${bash}/bin/bash

    enforce_vpn() {
        VPN_STATUS=$(${systemd}/bin/systemctl status openvpn-jobvpn.service)
        if [[ "$VPN_STATUS" == "inactive" ]]; then
            ${dunst}/bin/dunstify -t 5000 -u critical "VPN is off, turn it on and retry"
            exit 1
        fi
    }

    enforce_vpn

    SWARM_NODES=(
    ${builtins.concatStringsSep "\n"
               (map (host: builtins.head host.hostNames)
                    (builtins.filter (host: host.swarm == true)
                                     config.job.extraHosts))}
    )
    SWARM_NODES_COUNT=''${#SWARM_NODES[@]}
    # SELECTED_NODE=''${SWARM_NODES[$(( ( RANDOM % $SWARM_NODES_COUNT ) ))]}
    SELECTED_NODE=${config.job.infra.dockerSwarmLeaderHost}

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
        STACKS=$(${openssh}/bin/ssh ${config.job.infra.defaultRemoteUser}@$SELECTED_NODE \
                                         "docker stack ls | awk '{if(NR>1)print $1}'" | \
                                         ${gawk}/bin/awk '{print $1}')
        for i in "''${STACKS[@]}"
        do
            echo "$i"
        done
    }

    show_stack_status() {
        STACK=$1
        ${openssh}/bin/ssh ${config.job.infra.defaultRemoteUser}@$SELECTED_NODE \
        "docker stack ps $STACK $(docker_stack_ps_params)" > /tmp/docker_stack_status
        ${yad}/bin/yad --filename /tmp/docker_stack_status --text-info
        rm /tmp/docker_stack_status
    }

    ask_for_stack_task() {
        STACK=$1
        TASKS=$(${openssh}/bin/ssh ${config.job.infra.defaultRemoteUser}@$SELECTED_NODE \
        "docker stack ps $STACK $(docker_stack_ps_params)" | awk '{if(NR>1)print $0}')
        SERVICE=$(${openssh}/bin/ssh ${config.job.infra.defaultRemoteUser}@$SELECTED_NODE \
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
                                                  ${config.job.infra.defaultRemoteUser}@$SELECTED_NODE \
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
