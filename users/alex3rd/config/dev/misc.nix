{ config, pkgs, lib, ... }:
with import ../../../../pkgs/util.nix {inherit lib config pkgs;};
with import ../../const.nix {inherit lib config pkgs;};
let
    hostTraitsIpOutputPosition = 4;
in
{
    programs = {
        mtr.enable = true;
        wireshark = {
            enable = true;
            package = pkgs.wireshark-qt;
        };
        wavemon.enable = true;
    };
    users.extraUsers."${userName}".extraGroups = [ "wireshark" ];

    nixpkgs.config.packageOverrides = {
        rofi_extra_hosts_traits = pkgs.writeShellScriptBin "rofi_extra_hosts_traits" ''
            ${listOfSetsToShellHashtable
                (unfoldListOfSetsByAttr
                    (config.job.extraHosts ++ config.misc.extraHosts)
                    "hostNames")
                "hostNames"
                "EXTRA_HOSTS"
                false}

            list_extra_hosts() {
                for i in "''${!EXTRA_HOSTS[@]}"
                do
                    echo "$i"
                done
            }

            main() {
                SELECTED_HOST=$( (list_extra_hosts) | ${pkgs.rofi}/bin/rofi -dmenu -p "Select" )
                if [ -n "$SELECTED_HOST" ]; then
                    RESULT="$SELECTED_HOST ''${EXTRA_HOSTS[$SELECTED_HOST]}"
                    RESULT_NEWLINES=$(echo $RESULT | ${pkgs.gnused}/bin/sed 's/ /\n/g' | \
                                                     ${pkgs.gnused}/bin/sed 's/${sedPlaceholderChar}/ /g')
                    IP=$(echo $RESULT | ${pkgs.gawk}/bin/awk '{print ${"$" + builtins.toString hostTraitsIpOutputPosition}}' | \
                                        ${pkgs.gnused}/bin/sed 's/${sedPlaceholderChar}/ /g')
                    echo "$RESULT_NEWLINES" > /tmp/extra_host
                    echo "$IP" | ${pkgs.gawk}/bin/awk '{print $2}'| tr -d '\n' | ${pkgs.xsel}/bin/xsel -i --clipboard
                    ${pkgs.yad}/bin/yad --filename /tmp/extra_host --text-info
                    rm /tmp/extra_host
                fi
            }

            main

            exit 0
        '';
        rofi_jnettop = pkgs.writeShellScriptBin "rofi_jnettop" ''
            . ${pkgs.misc_lib}/bin/misc_lib

            main() {
                HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Host" )
                if [ -n "$HOST" ]; then
                    ${pkgs.tmux}/bin/tmux new-window "${pkgs.eternal-terminal}/bin/et \
                    ${config.job.infra.defaultRemoteUser}@$HOST -c 'jnettop'"
                fi
            }

            enforce_vpn

            main

            exit 0
        '';
    };
}
