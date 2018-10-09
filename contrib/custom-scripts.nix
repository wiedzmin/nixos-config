{config, pkgs, ...}:

{
    imports = [
        ../private/hometraits.nix
    ];
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
                        st="="
                        ;;
                    discharging)
                        st="▼"
                        ;;
                    charging)
                        st="▲"
                        ;;
                esac
                echo $st$UPOWER_PERCENTAGE
            '';
            status_cpu_temp = pkgs.writeShellScriptBin "status_cpu_temp" ''
                t=$(${pkgs.lm_sensors}/bin/sensors | ${pkgs.gawk}/bin/awk '/Core\ 0/ {gsub(/\+/,"",$3); gsub(/\..+/,"",$3)    ; print $3}')
                tc=$C0
                case 1 in
                    $((t <= 50)))
                        tc=$C2
                        ;;
                    $((t >= 87)))
                        tc=$C3
                        ;;
                    $((t >= 105)))
                        tc=$C4
                        ;;
                esac
                echo "$t°C"
            '';
            status_uptime = pkgs.writeShellScriptBin "status_uptime" ''
                ${pkgs.coreutils}/bin/uptime | ${pkgs.coreutils}/bin/cut -f 4-7 -d " " | ${pkgs.coreutils}/bin/cut -f 1-2 -d ","
            '';
            zip2targz = pkgs.writeShellScriptBin "zip2targz" ''
                tmpdir=`${pkgs.coreutils}/bin/mktemp -d`
                #Copy the zip to the temporary directory
                ${pkgs.coreutils}/bin/cp "$1" $tmpdir/
                #Unzip
                (cd $tmpdir && ${pkgs.unzip}/bin/unzip -q "$1")
                #Remove the original zipfile because we don't want that to be tar'd
                ${pkgs.coreutils}/bin/rm "$tmpdir/$1"
                #Tar the files
                outfilename=$(echo "$1" | ${pkgs.util-linux}/bin/rev | ${pkgs.coreutils}/bin/cut -d. -f2- | ${pkgs.util-linux}/bin/rev).tar
                (cd $tmpdir && ${pkgs.gnutar}/bin/tar cf "$outfilename" *)
                ${pkgs.coreutils}/bin/mv "$tmpdir/$outfilename" .
                ${pkgs.gzip}/bin/gzip ./$outfilename
                #Remove the temporary directory
                ${pkgs.coreutils}/bin/rm -rf $tmpdir
                #Print what we did
                echo "Converted $1 to $outfilename.gz"
            '';
            # see https://blog.jeaye.com/2017/07/30/nixos-revisited/
            optimize-nix = pkgs.writeShellScriptBin "optimize-nix" ''
                set -eu

                # Delete everything from this profile that isn't currently needed
                nix-env --delete-generations old

                # Delete generations older than a week
                nix-collect-garbage
                nix-collect-garbage --delete-older-than 7d

                # Optimize
                nix-store --gc --print-dead
                nix-store --optimise
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
            wifi-status = pkgs.writeShellScriptBin "wifi-status" ''
                essid=`${pkgs.wirelesstools}/bin/iwgetid -r`
                strength=`awk 'NR==3 {print $3 "00%"}' /proc/net/wireless`
                echo $essid: $strength
            '';
            systemctl-status = pkgs.writeShellScriptBin "systemctl-status" ''
                if [ -z "$1" ]
                then
                    echo -e ""
                else
                    status=`${pkgs.systemd}/bin/systemctl status $1 | awk 'NR==3 {print $2}'`
                    if [ $status == "inactive" ]
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
            tmux-sessions = pkgs.writeShellScriptBin "tmux-sessions" ''
                #!${pkgs.bash}/bin/bash

                # if not inside a tmux session, and if no session is started, initialize sessions
                if [ -z "$TMUX" ]; then
                    ${pkgs.tmux}/bin/tmux has-session -t 'work' 2>/dev/null
                    if [ "$?" -eq 1 ] ; then
                        ${pkgs.tmux}/bin/tmux new-session -d -s 'work' -d 'mc'
                    fi
                    ${pkgs.tmux}/bin/tmux has-session -t 'housekeeping' 2>/dev/null
                    if [ "$?" -eq 1 ] ; then
                        ${pkgs.tmux}/bin/tmux new-session -d -s 'housekeeping'
                    fi
                    ${pkgs.tmux}/bin/tmux has-session -t 'remote' 2>/dev/null
                    if [ "$?" -eq 1 ] ; then
                        ${pkgs.tmux}/bin/tmux new-session -d -s 'remote'
                    fi
                    ${pkgs.tmux}/bin/tmux attach -t housekeeping
                fi
            '';
            alacritty-tmux = pkgs.writeShellScriptBin "alacritty-tmux" ''
                ${pkgs.alacritty}/bin/alacritty -e tmux-sessions
            '';
            rescale-wallpaper = pkgs.writeShellScriptBin "rescale-wallpaper" ''
                ${pkgs.feh}/bin/feh --bg-fill ${config.x11.wallpapers_dir}/${config.x11.current_wallpaper}
            '';
            shell-capture = pkgs.writeShellScriptBin "shell-capture" ''
                TEMPLATE="$1"
                if [[ ! -n $TEMPLATE ]]
                then
                    exit 1
                fi
                TITLE="$*"
                if [[ -n $TMUX ]]
                then
                    TITLE=$(tmux display-message -p '#S')
                    tmux send -X copy-pipe-and-cancel "xclip -i -selection primary"
                fi

                if [[ -n $TITLE ]]
                then
                    emacsclient -n "org-protocol://capture?template=$TEMPLATE&title=$TITLE"
                else
                    emacsclient -n "org-protocol://capture?template=$TEMPLATE"
                fi
            '';
        };
    };
}
