{config, pkgs, ...}:

{
    config = {
        nixpkgs.config.packageOverrides = super: {
            mount_nas_volume = pkgs.writeShellScriptBin "mount_nas_volume" ''
                NAS_ONLINE=$(${pkgs.netcat}/bin/nc -z ${config.fs.storage.hostname} 22 2 -w 2 2>&1)
                if [ -z "$NAS_ONLINE" ]; then
                    ${pkgs.libnotify}/bin/notify-send -t 7000 -u critical "Cannot access NAS, network error"
                    exit 1
                fi

                VOLUME=$1
                ALREADY_MOUNTED=$(cat /etc/mtab | grep catscan | cut -d ' '  -f 1 | grep $VOLUME)
                if [[ ! -z $ALREADY_MOUNTED ]]; then
                    ${pkgs.libnotify}/bin/notify-send -t 5000 -u critical "Volume '$VOLUME' already mounted"
                    exit 1
                fi
                mkdir -p ${config.fs.storage.local_mount_base}/$VOLUME
                ${pkgs.afpfs-ng}/bin/mount_afp \
                    afp://${config.fs.storage.primary_user}:${config.fs.storage.primary_user_password}@${config.fs.storage.hostname}/$VOLUME \
                    ${config.fs.storage.local_mount_base}/$VOLUME
                if [[ $? -eq 0 ]]; then
                    ${pkgs.libnotify}/bin/notify-send -t 3000 "Volume '$VOLUME' succesfully mounted"
                else
                    ${pkgs.libnotify}/bin/notify-send -t 5000 -u critical "Error mounting volume '$VOLUME'"
                fi
            '';
            unmount_nas_volume = pkgs.writeShellScriptBin "unmount_nas_volume" ''
                VOLUME=$1
                YET_MOUNTED=$(cat /etc/mtab | grep catscan | cut -d ' '  -f 1 | grep $VOLUME)
                if [[ ! -z $YET_MOUNTED ]]; then
                    fusermount -u ${config.fs.storage.local_mount_base}/$VOLUME
                    ${pkgs.libnotify}/bin/notify-send -t 3000 "Volume $VOLUME succesfully unmounted!"
                else
                    ${pkgs.libnotify}/bin/notify-send -t 7000 "Volume '$VOLUME' already unmounted!"
                fi
            '';
            rofi_mount_nas_volume = pkgs.writeShellScriptBin "rofi_mount_nas_volume" ''
                nas_volumes=(
                ${builtins.concatStringsSep "\n" config.misc.nas_volumes}
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
                mounted_nas_volumes=$(cat /etc/mtab | grep ${config.fs.storage.hostname} | cut -d ' '  -f 1)
                for i in "''${mounted_nas_volumes[@]}"
                do
                    ${pkgs.unmount_nas_volume}/bin/unmount_nas_volume "$i"
                done
            '';
       };
    };
}
