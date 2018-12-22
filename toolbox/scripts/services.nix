{config, pkgs, ...}:

let
    volumeAmount = 10;
    backlightAmount = 10;
in
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            start_service = pkgs.writeShellScriptBin "start_service" ''
                SERVICE=$1
                USER=$2
                if [ -z "$SERVICE" ]; then
                    exit 1
                fi
                if [ -z "$USER" ]; then
                    ${pkgs.systemd}/bin/systemctl restart $SERVICE.service
                else
                    ${pkgs.systemd}/bin/systemctl --user restart $SERVICE.service
                fi
            '';
            stop_service = pkgs.writeShellScriptBin "stop_service" ''
                SERVICE=$1
                USER=$2
                if [ -z "$SERVICE" ]; then
                    exit 1
                fi
                if [ -z "$USER" ]; then
                    ${pkgs.systemd}/bin/systemctl stop $SERVICE.service
                else
                    ${pkgs.systemd}/bin/systemctl --user stop $SERVICE.service
                fi
            '';
            sshuttlectl = pkgs.writeShellScriptBin "sshuttlectl" ''
                 ACTION=$1
                 if [ ! -z "$ACTION" ]; then
                     case "$ACTION" in
                         start)
                             ${pkgs.start_service}/bin/start_service sshuttle
                             ;;
                         stop)
                             ${pkgs.stop_service}/bin/stop_service sshuttle
                             ;;
                         *)
                             echo "Unknown action: $ACTION"
                             exit 1
                             ;;
                     esac
                     exit 0
                 fi
                 exit 1
            '';
            jobvpnctl = pkgs.writeShellScriptBin "jobvpnctl" ''
                 ACTION=$1
                 if [ ! -z "$ACTION" ]; then
                     case "$ACTION" in
                         start)
                             ${pkgs.start_service}/bin/start_service openvpn-jobvpn
                             ;;
                         stop)
                             ${pkgs.stop_service}/bin/stop_service openvpn-jobvpn
                             ;;
                         *)
                             echo "Unknown action: $ACTION"
                             exit 1
                             ;;
                     esac
                     exit 0
                 fi
                 exit 1
            '';
            volumectl = pkgs.writeShellScriptBin "volumectl" ''
                 ACTION=$1
                 AMOUNT=''${2:-${toString volumeAmount}}%
                 if [ ! -z "$ACTION" ]; then
                     case "$ACTION" in
                         inc)
                             ${pkgs.pulseaudio}/bin/pactl set-sink-volume 0 +$AMOUNT
                             ;;
                         dec)
                             ${pkgs.pulseaudio}/bin/pactl set-sink-volume 0 -$AMOUNT
                             ;;
                         tog)
                             ${pkgs.pulseaudio}/bin/pactl set-sink-mute 0 toggle
                             ;;
                         *)
                             echo "Unknown action: $ACTION"
                             exit 1
                             ;;
                     esac
                     exit 0
                 fi
                 exit 1
            '';
            backlightctl = pkgs.writeShellScriptBin "backlightctl" ''
                 ACTION=$1
                 AMOUNT=''${2:-${toString backlightAmount}}
                 if [ ! -z "$ACTION" ]; then
                     case "$ACTION" in
                         inc)
                             ${pkgs.light}/bin/light -A $AMOUNT
                             ;;
                         dec)
                             ${pkgs.light}/bin/light -U $AMOUNT
                             ;;
                         full)
                             ${pkgs.light}/bin/light -S 100
                             ;;
                         *)
                             echo "Unknown action: $ACTION"
                             exit 1
                             ;;
                     esac
                     exit 0
                 fi
                 exit 1
            '';
            wifictl = pkgs.writeShellScriptBin "wifictl" ''
                 ACTION=$1
                 if [ ! -z "$ACTION" ]; then
                     case "$ACTION" in
                         on)
                             ${pkgs.networkmanager}/bin/nmcli radio wifi on
                             ;;
                         off)
                             ${pkgs.networkmanager}/bin/nmcli radio wifi off
                             ;;
                         jog)
                             ${pkgs.networkmanager}/bin/nmcli radio wifi off &&
                             ${pkgs.networkmanager}/bin/nmcli radio wifi on
                             ;;
                         *)
                             echo "Unknown action: $ACTION"
                             exit 1
                             ;;
                     esac
                     exit 0
                 fi
                 exit 1
            '';
        };
    };
}
