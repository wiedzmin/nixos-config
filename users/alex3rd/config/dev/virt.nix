{ config, pkgs, lib, ... }:
with import ../../../../util.nix {inherit lib config pkgs;};
with import ../../const.nix {inherit config pkgs;};
let
    dockerStackPsCustomFormat = "{{.Name}}   {{.Image}}   {{.Node}} {{.DesiredState}}   {{.CurrentState}}";
    useDockerStackPsCustomFormat = false;
    dockerStackShowOnlyRunning = true;
in
{
    virtualisation.docker = {
        enable = true;
        storageDriver = "overlay2";

    };
    virtualisation.libvirtd.enable = true;
    virtualisation.virtualbox.host.enable = true;
    virtualisation.virtualbox.host.enableExtensionPack = true;

    users.extraUsers."${userName}".extraGroups = [ "docker" "libvirtd" "vboxusers" ];

    nixpkgs.config.packageOverrides = super: {
        docker-machine-export = pkgs.writeShellScriptBin "docker-machine-export" ''
            if [ -z "$1" ]; then
              echo "Usage: machine-export.sh MACHINE_NAME"
              echo ""
              echo "Exports the specified docker-machine to a MACHINE_NAME.zip file"
              echo "Note: This script requires you to have the same \$MACHINE_STORAGE_PATH/certs available on all host systems"
              exit 0
            fi

            machine_name=$1

            ${pkgs.docker-machine}/bin/docker-machine status $machine_name 2>&1 > /dev/null
            if [ $? -ne 0 ]; then
              echo "No such machine found"
              exit 1
            fi

            set -e

            MACHINE_STORAGE_PATH="$(MACHINE_STORAGE_PATH:-"$HOME/.docker/machine")"
            machine_path="$MACHINE_STORAGE_PATH/machines/$machine_name"
            tmp_path="/tmp/machine-export-$(date +%s%3)"

            # copy to /tmp and strip out $MACHINE_STORAGE_PATH
            ${pkgs.coreutils}/bin/mkdir -p $tmp_path
            ${pkgs.coreutils}/bin/cp -r "$machine_path" "$tmp_path"
            ${pkgs.perl}/bin/perl -pi -e "s|$MACHINE_STORAGE_PATH|__MACHINE__STORAGE_PATH__|g" $tmp_path/$machine_name/config.json

            # create zip
            ${pkgs.coreutils}/bin/rm -f "$machine_name.zip"
            ${pkgs.zip}/bin/zip -rj "$machine_name.zip" "$tmp_path/$machine_name" > /dev/null

            echo "Exported machine to $machine_name.zip"

            # cleanup
            ${pkgs.coreutils}/bin/rm -rf $tmp_path
        '';
        docker-machine-import = pkgs.writeShellScriptBin "docker-machine-import" ''
            set -e

            if [ -z "$1" ]; then
              echo "Usage: docker-machine-import.sh MACHINE_NAME.zip"
              echo ""
              echo "Imports an exported machine from a MACHINE_NAME.zip file"
              echo "Note: This script requires you to have the same \$MACHINE_STORAGE_PATH/certs available on all host systems"
              exit 0
            fi

            machine_archive="$1"
            machine_name="$machine_archive/.zip/"
            MACHINE_STORAGE_PATH="$(MACHINE_STORAGE_PATH:-"$HOME/.docker/machine")"
            machine_path="$MACHINE_STORAGE_PATH/machines/$machine_name"

            if [ -d "$machine_path" ]; then
              echo "$machine_name already exists"
              exit 1
            fi

            ${pkgs.coreutils}/bin/rm -rf "$machine_name"
            ${pkgs.unzip}/bin/unzip "$machine_archive" -d "$machine_name" > /dev/null
            ${pkgs.perl}/bin/perl -pi -e "s|__MACHINE__STORAGE_PATH__|$MACHINE_STORAGE_PATH|g" $machine_name/config.json
            ${pkgs.coreutils}/bin/mv "$machine_name" "$MACHINE_STORAGE_PATH/machines"

            echo "Imported $machine_name to docker-machine ($machine_path)"
        '';
        # TODO: think of decoupling from job infra
        rofi_docker_stacks_info = pkgs.writeShellScriptBin "rofi_docker_stacks_info" ''
            . ${pkgs.misc_lib}/bin/misc_lib

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
                STACKS=$(${pkgs.openssh}/bin/ssh ${config.job.infra.defaultRemoteUser}@$SELECTED_NODE \
                                                 "docker stack ls | awk '{if(NR>1)print $1}'" | \
                                                 ${pkgs.gawk}/bin/awk '{print $1}')
                for i in "''${STACKS[@]}"
                do
                    echo "$i"
                done
            }

            show_stack_status() {
                STACK=$1
                ${pkgs.openssh}/bin/ssh ${config.job.infra.defaultRemoteUser}@$SELECTED_NODE \
                "docker stack ps $STACK $(docker_stack_ps_params)" > /tmp/docker_stack_status
                ${pkgs.yad}/bin/yad --filename /tmp/docker_stack_status --text-info
                rm /tmp/docker_stack_status
            }

            ask_for_stack_task() {
                STACK=$1
                TASKS=$(${pkgs.openssh}/bin/ssh ${config.job.infra.defaultRemoteUser}@$SELECTED_NODE \
                "docker stack ps $STACK $(docker_stack_ps_params)" | awk '{if(NR>1)print $0}')
                SERVICE=$(${pkgs.openssh}/bin/ssh ${config.job.infra.defaultRemoteUser}@$SELECTED_NODE \
                "docker service ls --format='{{.Name}}' | grep $STACK ")
                TASKS="''${SERVICE}
            ''${TASKS}"
                for i in "''${TASKS[@]}"
                do
                    echo "$i"
                done
            }

            main() {
                MODE=$( (ask_for_mode) | ${pkgs.rofi}/bin/rofi -dmenu -p "Mode" )
                STACK=$( (ask_for_stack) | ${pkgs.rofi}/bin/rofi -dmenu -p "Stack" )
                case "$MODE" in
                    status)
                        show_stack_status $STACK
                        ;;
                    logs)
                        TASK=$( (ask_for_stack_task $STACK) | ${pkgs.rofi}/bin/rofi -dmenu -p "Task" | ${pkgs.gawk}/bin/awk '{print $1}' )
                        ${pkgs.tmux}/bin/tmux new-window "${pkgs.eternal-terminal}/bin/et \
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
        '';
        rofi_remote_docker_logs = pkgs.writeShellScriptBin "rofi_remote_docker_logs" ''
            # TODO: abstract away creds/hosts details and rework accordingly
            . ${pkgs.misc_lib}/bin/misc_lib

            enforce_vpn

            ask_for_logs() {
                LOGS=$(${pkgs.openssh}/bin/ssh ${config.job.infra.defaultRemoteUser}@${config.job.infra.logsHost} "find ${config.job.infra.remoteDockerLogsPath}/ -maxdepth 1 -size +0 -type f | grep -v gz")
                for i in "''${LOGS[@]}"
                do
                    echo "$i"
                done
            }

            main() {
                LOG=$( (ask_for_logs) | ${pkgs.rofi}/bin/rofi -dmenu -p "View log" )
                if [ -n "$LOG" ]; then
                   ${pkgs.tmux}/bin/tmux new-window "${pkgs.eternal-terminal}/bin/et \
                   ${config.job.infra.defaultRemoteUser}@${config.job.infra.logsHost} \
                   -c 'tail -f $LOG'"
                fi
            }

            main

            exit 0
        '';
        rofi_containerized_services_discovery = pkgs.writeShellScriptBin "rofi_containerized_services_discovery" ''
            # TODO: think how to restrict networks/ports output (maybe pick first ones)
            main() {
                eval $(${pkgs.docker-machine}/bin/docker-machine env -u) # ensure we cosidering only local containers
                SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps --format '{{.Names}}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Container" )
                if [ ! -z "$SELECTED_CONTAINER" ]; then
                    CONTAINER_IP=$(${pkgs.docker}/bin/docker inspect $SELECTED_CONTAINER --format='{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.IPAddress}}{{end}}')
                    EXPOSED_PORT=$(${pkgs.docker}/bin/docker inspect $SELECTED_CONTAINER --format='{{range $port, $mappings :=.NetworkSettings.Ports}}{{$port}}{{end}}' | ${pkgs.coreutils}/bin/cut -f1 -d/)
                    ${firefoxOpenPageCmd} http://$CONTAINER_IP:$EXPOSED_PORT
                fi
            }

            main

            exit 0
        '';
        rofi_ctop = pkgs.writeShellScriptBin "rofi_ctop" ''
            . ${pkgs.misc_lib}/bin/misc_lib

            main() {
                HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Host" )
                if [ -n "$HOST" ]; then
                    enforce_vpn
                    ${pkgs.tmux}/bin/tmux new-window "${pkgs.eternal-terminal}/bin/et \
                    ${config.job.infra.defaultRemoteUser}@$HOST -c 'ctop'"
                fi
            }

            main

            exit 0
        '';
    };

    environment.systemPackages = with pkgs; [
        appimage-run
        ctop
        dive
        docker-machine
        docker_compose
        libcgroup
        promoter
        skopeo
        vagrant

        docker-machine-export
        docker-machine-import
    ];
}
