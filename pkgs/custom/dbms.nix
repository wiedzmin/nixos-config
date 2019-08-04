{ bash, config, coreutils, dunst, mycli, pass, pgcli, rofi, stdenv, systemd, tmux, ... }:
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

    declare -A DBMS_TRAITS

    DBMS_TRAITS=(
    ${builtins.concatStringsSep "\n"
      (stdenv.lib.mapAttrsToList
          (alias: meta: "  [\"${alias}\"]=\"${meta.ip} ${meta.command} ${meta.user} ${meta.passwordPassPath}\"")
            (config.job.dbmsTraits))}
    )

    MYCLI_BINARY=${mycli}/bin/mycli
    PGCLI_BINARY=${pgcli}/bin/pgcli

    list_dbms_traits() {
        for i in "''${!DBMS_TRAITS[@]}"
        do
            echo "$i"
        done
    }

    main() {
        DBMS_META="''${DBMS_TRAITS[$( (list_dbms_traits) | ${rofi}/bin/rofi -dmenu -p "Connect" )]}"
        if [ -n "$DBMS_META" ]; then
            DBMS_IP=$(echo $DBMS_META | ${coreutils}/bin/cut -f1 -d\ )
            DBMS_COMMAND=$(echo $DBMS_META | ${coreutils}/bin/cut -f2 -d\ )
            DBMS_USER=$(echo $DBMS_META | ${coreutils}/bin/cut -f3 -d\ )
            DBMS_PASSWORD_PASS_PATH=$(echo $DBMS_META | ${coreutils}/bin/cut -f4 -d\ )
            DBMS_PASSWORD=$(${pass}/bin/pass $DBMS_PASSWORD_PASS_PATH)
            CLI_BINARY_VARNAME="''${DBMS_COMMAND^^}_BINARY"
            CLI_EXECUTABLE="''${!CLI_BINARY_VARNAME}"
            if [ "$DBMS_COMMAND" == "mycli" ]; then
                ${tmux}/bin/tmux new-window "$CLI_EXECUTABLE --host $DBMS_IP --user $DBMS_USER --password $DBMS_PASSWORD"
            elif [ "$DBMS_COMMAND" == "pgcli" ]; then
                ${tmux}/bin/tmux new-window "PGPASSWORD=$DBMS_PASSWORD $CLI_EXECUTABLE --host $DBMS_IP --user $DBMS_USER"
            fi
        fi
    }

    main

    exit 0
''
