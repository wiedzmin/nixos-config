{ bash, config, coreutils, docker, docker-machine, dunst, eternal-terminal, gawk, rofi, systemd, tmux, ... }:
let
    dockerContainerShellExecutable = "/bin/bash";
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

    main() {
        HOST=$( cat /etc/hosts | ${gawk}/bin/awk '{print $2}' | ${coreutils}/bin/uniq | ${rofi}/bin/rofi -dmenu -p "Host" )
        if [ -n $HOST ]; then
            if [ "$HOST" == "localhost" ]; then
                eval $(${docker-machine}/bin/docker-machine env -u)
            else
                enforce_vpn
                eval $(${docker-machine}/bin/docker-machine env $HOST)
            fi
            SELECTED_CONTAINER=$( ${docker}/bin/docker ps --format '{{.Names}}' | ${rofi}/bin/rofi -dmenu -p "Container" )
            if [ -n "$SELECTED_CONTAINER" ]; then
                ${tmux}/bin/tmux new-window "${eternal-terminal}/bin/et \
                ${config.job.infra.defaultRemoteUser}@$HOST \
                -c 'docker exec -it $SELECTED_CONTAINER ${dockerContainerShellExecutable}'"
            fi
        fi
    }

    main

    exit 0
''
