{ bash, config, dunst, eternal-terminal, gawk, rofi, systemd, tmux, ... }: # TODO: think of decoupling from job infra
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
        HOST=$( cat /etc/hosts | ${gawk}/bin/awk '{print $2}' | ${rofi}/bin/rofi -dmenu -p "Host" )
        if [ -n "$HOST" ]; then
            enforce_vpn
            ${tmux}/bin/tmux new-window "${eternal-terminal}/bin/et \
            ${config.job.infra.defaultRemoteUser}@$HOST -c 'ctop'"
        fi
    }

    main

    exit 0
''
