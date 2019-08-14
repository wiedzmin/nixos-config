{ bash, config, dunst, eternal-terminal, lib, openssh, pkgs, rofi, systemd, tmux, ... }:
# TODO: think of decoupling from job infra
with import ../secrets/const.nix {inherit lib config pkgs;};
''
    #!${bash}/bin/bash

    # TODO: abstract away creds/hosts details and rework accordingly
    enforce_vpn() {
        VPN_STATUS=$(${systemd}/bin/systemctl status openvpn-jobvpn.service)
        if [[ "$VPN_STATUS" == "inactive" ]]; then
            ${dunst}/bin/dunstify -t 5000 -u critical "VPN is off, turn it on and retry"
            exit 1
        fi
    }

    enforce_vpn

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
           ${tmux}/bin/tmux new-window "${eternal-terminal}/bin/et \
           ${jobInfraLogsHost} \
           -c 'tail -f $LOG'"
        fi
    }

    main

    exit 0
''
