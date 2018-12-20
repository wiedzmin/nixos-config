{config, pkgs, ...}:

{
    config = {
        nixpkgs.config.packageOverrides = super: {
            hddtemp-script = pkgs.writeShellScriptBin "hddtemp-script" ''
                ${pkgs.netcat}/bin/nc localhost 7634 | ${pkgs.gawk}/bin/awk -F\| '{print ($4)}'
            '';
            status_bat_info = pkgs.writeShellScriptBin "status_bat_info" ''
                BAT_NAME="BAT0"
                UPOWER_ENERGY_FULL_DESIGN=$(${pkgs.upower}/bin/upower -i /org/freedesktop/UPower/devices/battery_$BAT_NAME | grep "energy-full-design:" | cut -f 14 -d " ")
                UPOWER_ENERGY_FULL_FACT=$(${pkgs.upower}/bin/upower -i /org/freedesktop/UPower/devices/battery_$BAT_NAME | grep "energy-full:" | cut -f 14 -d " ")
                UPOWER_PERCENTAGE=$(${pkgs.upower}/bin/upower -i /org/freedesktop/UPower/devices/battery_$BAT_NAME | grep "percentage:" | cut -f 15 -d " ")
                UPOWER_STATE=$(${pkgs.upower}/bin/upower -i /org/freedesktop/UPower/devices/battery_$BAT_NAME | grep "state:" | cut -f 20 -d " ")
                case $UPOWER_STATE in
                    fully-charged)
                        ST="="
                        ;;
                    discharging)
                        ST="▼"
                        ;;
                    charging)
                        ST="▲"
                        ;;
                esac
                echo $ST$UPOWER_PERCENTAGE
            '';
            status_uptime = pkgs.writeShellScriptBin "status_uptime" ''
                ${pkgs.procps}/bin/w | ${pkgs.gnused}/bin/sed -r '1 s/.*up *(.*),.*user.*/\1/g;q'
            '';
            wifi-status = pkgs.writeShellScriptBin "wifi-status" ''
                ESSID=`${pkgs.wirelesstools}/bin/iwgetid -r`
                STRENGTH=$((`awk 'NR==3 {print substr($3, 1, length($3)-1)}' /proc/net/wireless`*100/70))
                QUALITY_COLOR=
                case 1 in
                    $((STRENGTH < 30)))
                        QUALITY_COLOR=red
                        ;;
                    $((STRENGTH >= 30 && STRENGTH < 70)))
                        QUALITY_COLOR=yellow
                        ;;
                    $((STRENGTH >= 70 && STRENGTH <= 100)))
                        QUALITY_COLOR=green
                        ;;
                esac
                echo $ESSID: "<fc=$QUALITY_COLOR>$STRENGTH</fc>%"
            '';
            systemctl-status = pkgs.writeShellScriptBin "systemctl-status" ''
                if [ -z "$1" ]
                then
                    echo -e ""
                else
                    STATUS=`${pkgs.systemd}/bin/systemctl status $1 | awk 'NR==3 {print $2}'`
                    if [ $STATUS == "inactive" ]
                    then
                        echo -e ""
                    else
                        if [ -z "$2" ]
                        then
                            echo -e "[*]"
                        else
                            echo -e $2
                        fi
                    fi
                fi
            '';
            maybe_ssh_host = pkgs.writeShellScriptBin "maybe_ssh_host" ''
                # tmux: pane_tty: pts/5
                PANE_TTY=$(${pkgs.tmux}/bin/tmux display-message -p '#{pane_tty}' | ${pkgs.coreutils}/bin/cut -c 6-)

                # get IP/hostname according to pane_tty
                REMOTE_SESSION=$(${pkgs.procps}/bin/pgrep -t $pane_tty -a -f "ssh " | ${pkgs.gawk}/bin/awk 'NF>1{print $NF}')

                if [[ "$REMOTE_SESSION" != "" ]]; then
                    echo $REMOTE_SESSION
                else
                    echo $(whoami)@$(${pkgs.nettools}/bin/hostname)
                fi
            '';
            show_uptime_info = pkgs.writeShellScriptBin "show_uptime_info" ''
                ${pkgs.libnotify}/bin/notify-send -t 7000 "Uptime: $(${pkgs.status_uptime}/bin/status_uptime)"
            '';
       };
    };
}
