{config, pkgs, lib, ...}:
with import ../util.nix {inherit lib config pkgs;};
let
    dockerContainerShellExecutable = "/bin/bash";
in
{
    config = {
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
            rofi_docker_container_traits = pkgs.writeShellScriptBin "rofi_docker_container_traits" ''
                declare -A CONTAINER_TRAITS

                CONTAINER_TRAITS=(
                  ["name"]='{{index (split .Name "/") 1}}'
                  ["created"]='{{.Created}}'
                  ["path + args"]='{{.Path}} :: {{.Args}}'
                  ["stats"]='{{println .State.Status}} {{.State.StartedAt}} <--> {{println .State.FinishedAt}} restarts: {{.RestartCount}}'
                  ["ports"]='{{range $port, $mappings :=.NetworkSettings.Ports}}{{$port}} --> {{range $ifnum, $ifdef:=$mappings}}{{$ifnum}}) {{$ifdef.HostIp}}:{{$ifdef.HostPort}}{{end}}{{end}}'
                  ["mounts"]='{{range $i, $mountpoint :=.Mounts}}{{with $mountpoint}}{{.Type}} {{.Destination}} --> {{.Source}} RW:{{.RW}}{{end}}{{end}}'
                  ["env"]='{{range $entry :=.Config.Env}}{{with $entry}}{{println .}}{{end}}{{end}}'
                  ["cmd"]='{{index .Config.Cmd 0}}'
                  ["image"]='{{.Config.Image}}'
                  ["volumes"]='{{range $vol, $data :=.Config.Volumes}}{{$vol}}: {{$data}}{{end}}'
                  ["entrypoint"]='{{index .Config.Entrypoint 0}}'
                  ["labels"]='{{range $name, $value :=.Config.Labels}}{{$name}}: {{println $value}}{{end}}'
                  ["net: ip"]='{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.IPAddress}}{{end}}'
                  ["net: gateway"]='{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.Gateway}}{{end}}'
                  ["net: names"]='{{range $network, $settings :=.NetworkSettings.Networks}}{{$network}}/{{println $settings.Aliases}}{{end}}'
                )

                CONTAINER_STATUSES=(
                  "alive"
                  "all"
                )

                list_container_statuses() {
                    for i in "''${CONTAINER_STATUSES[@]}"
                    do
                        echo "$i"
                    done
                }

                list_container_traits() {
                    for i in "''${!CONTAINER_TRAITS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/uniq | ${pkgs.rofi}/bin/rofi -dmenu -p "Host" )
                    if [ ! -z "$HOST" ]; then
                        if [ "$HOST" == "localhost" ]; then
                            eval $(${pkgs.docker-machine}/bin/docker-machine env -u)
                        else
                            eval $(${pkgs.docker-machine}/bin/docker-machine env $HOST)
                        fi
                        CONTAINER_STATUS=$( (list_container_statuses) | ${pkgs.rofi}/bin/rofi -dmenu -p "Status" )
                        if [ -z "$CONTAINER_STATUS" ]; then
                            exit 1
                        fi
                        if [ "$CONTAINER_STATUS" == "all" ]; then
                            SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps -a --format '{{.Names}}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Container" )
                        else
                            SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps --format '{{.Names}}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Container" )
                        fi
                        if [ -n "$SELECTED_CONTAINER" ]; then
                            SELECTED_TRAIT=$( (list_container_traits) | ${pkgs.rofi}/bin/rofi -dmenu -p "Inspect" )
                            if [ -n "$SELECTED_TRAIT" ]; then
                                INSPECT_COMMAND="${pkgs.docker}/bin/docker inspect $SELECTED_CONTAINER --format='"''${CONTAINER_TRAITS[$SELECTED_TRAIT]}"'"
                                eval `echo $INSPECT_COMMAND` | tr -d '\n' | ${pkgs.xsel}/bin/xsel -i --clipboard
                                eval `echo $INSPECT_COMMAND` > /tmp/docker_traits
                                ${pkgs.yad}/bin/yad --filename /tmp/docker_traits --text-info
                                rm /tmp/docker_traits
                            fi
                        fi
                    fi
                }

                main

                exit 0
            '';
            rofi_docker_shell = pkgs.writeShellScriptBin "rofi_docker_shell" ''
                . ${pkgs.misc_lib}/bin/misc_lib

                main() {
                    HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/uniq | ${pkgs.rofi}/bin/rofi -dmenu -p "Host" )
                    if [ -n $HOST ]; then
                        if [ "$HOST" == "localhost" ]; then
                            eval $(${pkgs.docker-machine}/bin/docker-machine env -u)
                        else
                            enforce_vpn
                            eval $(${pkgs.docker-machine}/bin/docker-machine env $HOST)
                        fi
                        SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps --format '{{.Names}}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Container" )
                        if [ -n "$SELECTED_CONTAINER" ]; then
                            ${pkgs.tmux}/bin/tmux new-window "${pkgs.eternal-terminal}/bin/et \
                            ${config.job.infra.defaultRemoteUser}@$HOST \
                            -c 'docker exec -it $SELECTED_CONTAINER ${dockerContainerShellExecutable}'"
                        fi
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
       };
    };
}
