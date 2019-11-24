{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.virtualization;
  dnsContainerName = "docker_dns";
  dlint = pkgs.writeShellScriptBin "dlint" ''
    if [ -z $1 ]; then
        echo "Dockerfile missing"
        exit 1
    fi

    ${pkgs.docker}/bin/docker run --rm -i -v $(realpath $1):/Dockerfile redcoolbeans/dockerlint
  '';
  hadolintd = pkgs.writeShellScriptBin "hadolintd" ''
    if [ -z $1 ]; then
        echo "Dockerfile missing"
        exit 1
    fi

    # TODO: add options such as help displaying
    # TODO: templatize yaml config (in nix)
    # see github for all the references
    ${pkgs.docker}/bin/docker run --rm -i -v $(realpath $1):/tmp/Dockerfile hadolint/hadolint hadolint /tmp/Dockerfile
  '';
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
  docker_containers_traits = pkgs.writeShellScriptBin "docker_containers_traits" ''
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

    function show_list() {
        contents=("$@")
        for i in "''${contents[@]}";
        do
            echo "$i"
        done
    }

    function show_mapping_keys() {
        eval "declare -A contents="''${1#*=}
        for i in "''${!contents[@]}";
        do
            echo "$i"
        done
    }

    main() {
        HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/uniq | ${pkgs.dmenu}/bin/dmenu -i -p "Host" -l 15)
        if [ ! -z "$HOST" ]; then
            if [ "$HOST" == "localhost" ]; then
                eval $(${pkgs.docker-machine}/bin/docker-machine env -u)
            else
                eval $(${pkgs.docker-machine}/bin/docker-machine env $HOST)
            fi
            CONTAINER_STATUS=$( (show_list "''${CONTAINER_STATUSES[@]}") | ${pkgs.dmenu}/bin/dmenu -i -p "Status" -l 15)
            if [ -z "$CONTAINER_STATUS" ]; then
                exit 1
            fi
            if [ "$CONTAINER_STATUS" == "all" ]; then
                SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps -a --format '{{.Names}}' | ${pkgs.dmenu}/bin/dmenu -i -p "Container" -l 15)
            else
                SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps --format '{{.Names}}' | ${pkgs.dmenu}/bin/dmenu -i -p "Container" -l 15)
            fi
            if [ -n "$SELECTED_CONTAINER" ]; then
                SELECTED_TRAIT=$( (show_mapping_keys "$(declare -p CONTAINER_TRAITS)") | ${pkgs.dmenu}/bin/dmenu -i -p "Inspect" -l 15)
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
  discover_containerized_services = pkgs.writeShellScriptBin "discover_containerized_services" ''
    # TODO: think how to restrict networks/ports output (maybe pick first ones)
    main() {
        eval $(${pkgs.docker-machine}/bin/docker-machine env -u) # ensure we cosidering only local containers
        SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps --format '{{.Names}}' | ${pkgs.dmenu}/bin/dmenu -i -p "Container" -l 15)
        if [ ! -z "$SELECTED_CONTAINER" ]; then
            CONTAINER_IP=$(${pkgs.docker}/bin/docker inspect $SELECTED_CONTAINER --format='{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.IPAddress}}{{end}}')
            EXPOSED_PORT=$(${pkgs.docker}/bin/docker inspect $SELECTED_CONTAINER --format='{{range $port, $mappings :=.NetworkSettings.Ports}}{{$port}}{{end}}' | ${coreutils}/bin/cut -f1 -d/)
            ${config.attributes.defaultCommands.browser} http://$CONTAINER_IP:$EXPOSED_PORT
        fi
    }

    main

    exit 0
  '';
  remote_docker_logs = pkgs.writeShellScriptBin "remote_docker_logs" ''
    # TODO: think of decoupling from job infra

    ask_for_logs() {
        LOGS=$(${pkgs.openssh}/bin/ssh ${config.secrets.job.infra.logsHost} "find ${config.secrets.job.infra.remoteDockerLogsRoot}/ -maxdepth 1 -size +0 -type f | grep -v gz")
        for i in "''${LOGS[@]}"
        do
            echo "$i"
        done
    }

    main() {
        LOG=$( (ask_for_logs) | ${pkgs.dmenu}/bin/dmenu -i -p "View log" -l 15)
        if [ -n "$LOG" ]; then
            enforce_job_vpn_up || exit 1
            ${pkgs.tmux}/bin/tmux new-window "${pkgs.openssh}/bin/ssh \
            ${config.secrets.job.infra.logsHost} 'tail -f $LOG'"
        fi
    }

    main

    exit 0
  '';
  docker_stacks_info = pkgs.writeShellScriptBin "docker_stacks_info" ''
    # TODO: think of decoupling from job infra

    enforce_job_vpn_up || exit 1

    SWARM_NODES=(
    ${builtins.concatStringsSep "\n"
    (map (host: builtins.head host.hostNames) (builtins.filter (host: host.swarm == true) jobExtraHosts))}
    )
    SWARM_LEADER_NODE=$(${openssh}/bin/ssh ${jobInfraSeedHost} "docker node ls --format '{{.Hostname}} {{ .ManagerStatus }}' | grep Leader | cut -f1 -d\ ")

    docker_stack_ps_params() {
        echo ${if docker.stacks.showOnlyRunning then ''--filter \"desired-state=Running\"'' else ""}
             ${if docker.stacks.useCustomFormat then " --format \\\"${docker.stacks.psCustomFormat}\\\"" else ""}
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
        STACKS=$(${pkgs.openssh}/bin/ssh $SWARM_LEADER_NODE \
                                         "docker stack ls | awk '{if(NR>1)print $1}'" | \
                                         ${pkgs.gawk}/bin/awk '{print $1}')
        for i in "''${STACKS[@]}"
        do
            echo "$i"
        done
    }

    show_stack_status() {
        STACK=$1
        ${pkgs.openssh}/bin/ssh $SWARM_LEADER_NODE \
        "docker stack ps $STACK $(docker_stack_ps_params)" > /tmp/docker_stack_status
        ${pkgs.yad}/bin/yad --filename /tmp/docker_stack_status --text-info
        rm /tmp/docker_stack_status
    }

    ask_for_stack_task() {
        STACK=$1
        TASKS=$(${pkgs.openssh}/bin/ssh $SWARM_LEADER_NODE \
        "docker stack ps $STACK $(docker_stack_ps_params)" | awk '{if(NR>1)print $0}')
        SERVICE=$(${pkgs.openssh}/bin/ssh $SWARM_LEADER_NODE \
        "docker service ls --format='{{.Name}}' | grep $STACK ")
        TASKS="''${SERVICE}
    ''${TASKS}"
        for i in "''${TASKS[@]}"
        do
            echo "$i"
        done
    }

    main() {
        MODE=$( (ask_for_mode) | ${pkgs.dmenu}/bin/dmenu -i -p "Mode" -l 15)
        STACK=$( (ask_for_stack) | ${pkgs.dmenu}/bin/dmenu -i -p "Stack" -l 15)
        case "$MODE" in
            status)
                show_stack_status $STACK
                ;;
            logs)
                TASK=$( (ask_for_stack_task $STACK) | ${pkgs.dmenu}/bin/dmenu -i -p "Task" -l 15 | ${pkgs.gawk}/bin/awk '{print $1}' )
                ${pkgs.tmux}/bin/tmux new-window "${pkgs.openssh}/bin/ssh $SWARM_LEADER_NODE \
                                                  'docker service logs --follow $TASK'"
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
  docker_stacks_info_new = pkgs.writeScriptBin "docker_stacks_info_new" ''
    #! /usr/bin/env nix-shell
    #! nix-shell -i python3 -p python3

    # NOTE: unfinished rewrite in Python, see original just above

    import os
    import subprocess
    import random

    endpoint_nodes = ["router", "elk1", "tech1", "tech2"] # TODO: provide from nix
    nodes_meta = {}

    while True:
        entry_node = random.choice(endpoint_nodes)
        swarm_nodes_task = subprocess.Popen('ssh {0} "docker node ls"'.format(entry_node),
                                            shell=True, stdout=subprocess.PIPE)
        nodes_lines = swarm_nodes_task.stdout.read().decode().split("\n")
        task_result = swarm_nodes_task.wait()

        if task_result != 0:
            continue

        for node in nodes_lines[1:-1]:
            node_fields = node.split()
            parts_count = len(node_fields)
            if parts_count == 5:
                nodes_meta[node_fields[1]] = {
                    "manager_status": "",
                    "status": node_fields[2],
                    "availability": node_fields[3]
                }
            elif parts_count == 6:
                nodes_meta[node_fields[1]] = {
                    "manager_status": node_fields[4],
                    "status": node_fields[2],
                    "availability": node_fields[3]
                }
            elif parts_count == 7:
                nodes_meta[node_fields[2]] = {
                    "manager_status": node_fields[5],
                    "status": node_fields[3],
                    "availability": node_fields[4]
                }
        print(nodes_meta)
        break
  '';
  vdi2qcow2 = pkgs.writeShellScriptBin "vdi2qcow2" ''
    ${pkgs.qemu}/bin/qemu-img convert -f vdi -O qcow2 $1 "''${1%.*}.qcow2"
  '';
  docker_shell = let
    dockerPsCommand = "docker ps --format '{{.Names}}'";
  in pkgs.writeShellScriptBin "docker_shell" ''
    main() {
        HOST=$( cat /etc/hosts | grep -v "::1" | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/uniq | ${pkgs.dmenu}/bin/dmenu -p "Host" -l 20)
        if [ -n $HOST ]; then
            if [ "$HOST" == "localhost" ]; then
                SELECTED_CONTAINER=$( ${dockerPsCommand} | ${pkgs.dmenu}/bin/dmenu -p "Container" -l 20)
            else
                enforce_job_vpn_up || exit 1
                SELECTED_CONTAINER=$( ${pkgs.openssh}/bin/ssh $HOST '${dockerPsCommand}' | ${pkgs.dmenu}/bin/dmenu -p "Container" -l 20)
            fi
            if [ -n "$SELECTED_CONTAINER" ]; then
                ${pkgs.tmux}/bin/tmux new-window "${pkgs.openssh}/bin/ssh $HOST \
                'docker exec -it $SELECTED_CONTAINER ${config.custom.virtualization.docker.defaultContainerShell}'"
            fi
        fi
    }

    main

    exit 0
  '';
  # docker_stacks_info = ./docker_stacks_info.nix; # temporarily disabled, rework after refactoring extraHosts
in {
  options = {
    custom.virtualization = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable virtualization";
      };
      docker.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Docker";
      };
      docker.aux.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Docker auxillary packages";
      };
      docker.defaultContainerShell = mkOption {
        type = types.str;
        default = "/bin/bash";
        description = "Default shell to execute within container with `exec -it`";
      };
      docker.storageDriver = mkOption {
        type = types.str;
        default = "overlay2";
        description = "Docker storage driver";
      };
      docker.devdns.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Docker internal DNS service";
      };
      docker.devdns.restartPolicy = mkOption {
        type = types.str;
        default = "unless-stopped";
        description = "DNS service restarting policy. See respective Docker docs for reference.";
      };
      docker.devdns.port = mkOption {
        type = types.str;
        default = "5380";
        description = "Port to bind DNS service on.";
      };
      docker.devdns.autoStart = mkOption {
        default = true;
        type = types.bool;
        description = "Whether DNS service should be started automatically.";
      };
      docker.stacks.psCustomFormat = mkOption {
        type = types.str;
        default = "{{.Name}}   {{.Image}}   {{.Node}} {{.DesiredState}}   {{.CurrentState}}";
        description = "Custom columns formatting for `docker stack ps` output.";
      };
      docker.stacks.useCustomFormat = mkOption {
        type = types.bool;
        default = false;
        description = "Wheteher to use custom formatting for `docker stack ps` output.";
      };
      docker.stacks.showOnlyRunning = mkOption {
        type = types.bool;
        default = true;
        description = "Show only running services in stack.";
      };
      libvirt.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Libvirt";
      };
      virtualbox.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable VirtualBox";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      environment.systemPackages = with pkgs; [
        nfs-utils # for vagrant
        tigervnc
        vagrant
      ];

      boot.kernel.sysctl = {
        "net.ipv4.ip_forward" = 1; # for VMs forwarding
      };
    })
    (mkIf (cfg.enable && cfg.virtualbox.enable) {
      virtualisation.virtualbox.host.enable = true;
      users.users."${config.attributes.mainUser.name}".extraGroups = [ "vboxusers" ];
    })
    (mkIf (cfg.enable && cfg.docker.enable && cfg.docker.aux.enable) {
      environment.systemPackages = with pkgs; [
        # docker-slim # TODO: make package https://github.com/docker-slim/docker-slim
        clair # https://werner-dijkerman.nl/2019/01/28/scanning-docker-images-with-coreos-clair/
        nsjail
        skopeo
      ];
    })

    (mkIf (cfg.enable && cfg.docker.enable) {
      virtualisation.docker = {
        enable = true;
        storageDriver = cfg.docker.storageDriver;
      };

      users.users."${config.attributes.mainUser.name}".extraGroups = [ "docker" ];

      custom.dev.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli del job/swarm_endpoint_hosts
        ${pkgs.redis}/bin/redis-cli lpush job/swarm_endpoint_hosts ${builtins.concatStringsSep " " config.secrets.job.infra.swarmEndpointHosts}
      '';

      home-manager.users."${config.attributes.mainUser.name}" = {
        xdg.configFile."hadolint.yaml".text = ''
          ignored:
            - DL3007
        '';
      };

      environment.systemPackages = with pkgs; [
        dlint
        docker-machine-export
        docker-machine-import
        docker_containers_traits
        docker_shell
        hadolintd
        vdi2qcow2
      ] ++ [
        ctop
        dive
        docker-machine
        docker_compose
        libcgroup
        promoter
        arion
      ];
    })

    (mkIf (cfg.enable && cfg.docker.enable && cfg.docker.devdns.enable) {
      systemd.services."docker-devdns" = {
        description = "Dev DNS for docker containers";
        wantedBy = [ (optionalString cfg.docker.devdns.autoStart "multi-user.target") ];
        after = [ "docker.service" "docker.socket" ];
        requires = [ "docker.service" "docker.socket" ];
        script = ''
          ${pkgs.docker}/bin/docker run \
                                    --restart=${cfg.docker.devdns.restartPolicy} \
                                    -p ${cfg.docker.devdns.port}:${cfg.docker.devdns.port} \
                                    --name=${dnsContainerName} \
                                    -v /var/run/docker.sock:/var/run/docker.sock \
                                    -v /etc/resolv.conf:/etc/resolv.conf \
                                    defreitas/dns-proxy-server
        '';
        preStop = "${pkgs.docker}/bin/docker stop ${dnsContainerName}";
        reload = "${pkgs.docker}/bin/docker restart ${dnsContainerName}";
        serviceConfig = {
          ExecStartPre = "-${pkgs.docker}/bin/docker rm -f ${dnsContainerName}";
          ExecStopPost = "-${pkgs.docker}/bin/docker rm -f ${dnsContainerName}";
          TimeoutStartSec = 0;
          TimeoutStopSec = 120;
          Restart = "always";
        };
      };
    })

    (mkIf (cfg.enable && cfg.libvirt.enable) {
      virtualisation.libvirtd = { enable = true; };
      virtualisation.kvmgt.enable = true;

      users.users."${config.attributes.mainUser.name}".extraGroups = [ "libvirtd" ];

      networking.nat.internalInterfaces = ["virbr0"];
      services.dnsmasq.extraConfig = ''
        except-interface=virbr0 # ignore virbr0 as libvirtd listens here
      '';

      boot.kernelParams = [
        "kvm.allow_unsafe_assigned_interrupts=1"
        "kvm.ignore_msrs=1"
        "kvm-intel.nested=1"
      ];
      boot.kernelModules = [ "kvm-intel" ];
      boot.extraModprobeConfig = ''
        options kvm-intel nested=1
      '';

      # required for usb redirection to work
      security.wrappers.spice-client-glib-usb-acl-helper.source =
        "${pkgs.spice_gtk}/bin/spice-client-glib-usb-acl-helper";

      environment.systemPackages = with pkgs; [
        kvm
        libvirt # for `vagrant plugin install vagrant-libvirt`
        qemu-utils
        spice
        spice-gtk
        virtmanager
        virtviewer
      ] ++ lib.optionals config.attributes.staging.enable [
        x11spice
      ];
    })
    (mkIf (cfg.docker.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "M-C-c" = ''spawn "${docker_containers_traits}/bin/docker_containers_traits" >> showWSOnProperScreen "shell"'';
        "M-C-d" = ''spawn "${docker_shell}/bin/docker_shell" >> showWSOnProperScreen "shell"'';
        # "M-C-l" = ''spawn "${custom.remote_docker_logs}/bin/remote_docker_logs" >> showWSOnProperScreen "shell"'';
        # "M-C-s" = ''spawn "{custom.docker_stacks_info}/bin/docker_stacks_info" >> showWSOnProperScreen "shell"'';
      };
    })
  ];
}
