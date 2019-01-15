{config, pkgs, ...}:

let
    nasConfigPath = "$HOME/.config/synology/nas.yml";
in
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            mount_nas_volume = pkgs.writeShellScriptBin "mount_nas_volume" ''
                CONFIGFILE=${nasConfigPath}
                if [[ ! -f $CONFIGFILE ]]; then
                    ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Missing config file, exiting"
                    exit 1
                fi
                NAS_HOSTNAME=$(${pkgs.shyaml}/bin/shyaml -gy nas.hostname $CONFIGFILE)
                NAS_ADMIN_LOGIN=$(${pkgs.shyaml}/bin/shyaml -gy nas.users.admin.login $CONFIGFILE)
                NAS_ADMIN_PASSWORD=$(${pkgs.shyaml}/bin/shyaml -gy nas.users.admin.password $CONFIGFILE)
                NAS_MOUNT_PATH=$(${pkgs.shyaml}/bin/shyaml -gy nas.mount.basedir $CONFIGFILE)

                NAS_ONLINE=$(${pkgs.netcat}/bin/nc -z $NAS_HOSTNAME 22 2 -w 2 2>&1)
                if [ -z "$NAS_ONLINE" ]; then
                    ${pkgs.dunst}/bin/dunstify -t 7000 -u critical "Cannot access NAS, network error"
                    exit 1
                fi

                VOLUME=$1
                ALREADY_MOUNTED=$(cat /etc/mtab | grep catscan | cut -d ' '  -f 1 | grep $VOLUME)
                if [[ ! -z $ALREADY_MOUNTED ]]; then
                    ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Volume '$VOLUME' already mounted"
                    exit 1
                fi
                mkdir -p $NAS_MOUNT_PATH/$VOLUME
                ${pkgs.afpfs-ng}/bin/mount_afp afp://$NAS_ADMIN_LOGIN:$NAS_ADMIN_PASSWORD@$NAS_HOSTNAME/$VOLUME \
                    $NAS_MOUNT_PATH/$VOLUME
                if [[ $? -eq 0 ]]; then
                    ${pkgs.dunst}/bin/dunstify -t 3000 "Volume '$VOLUME' succesfully mounted"
                else
                    ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Error mounting volume '$VOLUME'"
                fi
            '';
            unmount_nas_volume = pkgs.writeShellScriptBin "unmount_nas_volume" ''
                CONFIGFILE=${nasConfigPath}
                if [[ ! -f $CONFIGFILE ]]; then
                    ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Missing config file, exiting"
                    exit 1
                fi
                NAS_MOUNT_PATH=$(${pkgs.shyaml}/bin/shyaml -gy nas.mount.basedir $CONFIGFILE)

                VOLUME=$1
                YET_MOUNTED=$(cat /etc/mtab | grep catscan | cut -d ' '  -f 1 | grep $VOLUME)
                if [[ ! -z $YET_MOUNTED ]]; then
                    fusermount -u $NAS_MOUNT_PATH/$VOLUME
                    ${pkgs.dunst}/bin/dunstify -t 3000 "Volume $VOLUME succesfully unmounted!"
                else
                    ${pkgs.dunst}/bin/dunstify -t 7000 "Volume '$VOLUME' already unmounted!"
                fi
            '';
            rofi_mount_nas_volume = pkgs.writeShellScriptBin "rofi_mount_nas_volume" ''
                CONFIGFILE=${nasConfigPath}
                if [[ ! -f $CONFIGFILE ]]; then
                    ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Missing config file, exiting"
                    exit 1
                fi
                NAS_VOLUMES=$(${pkgs.shyaml}/bin/shyaml -gy nas.volumes $CONFIGFILE)

                nas_volumes=(
                $NAS_VOLUMES
                )

                list_nas_volumes() {
                    for i in "''${nas_volumes[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    selected_volume=$( (list_nas_volumes) | ${pkgs.rofi}/bin/rofi -dmenu -p "Mount: " )
                    if [ -n "$selected_volume" ]; then
                        ${pkgs.mount_nas_volume}/bin/mount_nas_volume "$selected_volume"
                    fi
                }

                main

                exit 0
            '';
            rofi_unmount_nas_volume = pkgs.writeShellScriptBin "rofi_unmount_nas_volume" ''
                mounted_nas_volumes=$(cat /etc/mtab | grep catscan | cut -d ' '  -f 1)

                list_mounted_volumes() {
                    for i in "''${mounted_nas_volumes[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    selected_volume=$( (list_mounted_volumes) | ${pkgs.rofi}/bin/rofi -dmenu -p "Unmount: " )
                    if [ -n "$selected_volume" ]; then
                        ${pkgs.unmount_nas_volume}/bin/unmount_nas_volume "$selected_volume"
                    fi
                }

                main

                exit 0
            '';
            force_unmount_nas = pkgs.writeShellScriptBin "force_unmount_nas" ''
                CONFIGFILE=${nasConfigPath}
                if [[ ! -f $CONFIGFILE ]]; then
                    ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Missing config file, exiting"
                    exit 1
                fi
                NAS_HOSTNAME=$(${pkgs.shyaml}/bin/shyaml -gy nas.hostname $CONFIGFILE)

                mounted_nas_volumes=$(cat /etc/mtab | grep $NAS_HOSTNAME | cut -d ' '  -f 1)
                for i in "''${mounted_nas_volumes[@]}"
                do
                    ${pkgs.unmount_nas_volume}/bin/unmount_nas_volume "$i"
                done
            '';
       };
    };
}
