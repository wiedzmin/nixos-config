{ afpfs-ng, bash, coreutils, dunst, gnugrep, netcat, rofi, shyaml, ... }:
let
    nasConfigPath = "$HOME/.config/synology/nas.yml";
in
''
    #!${bash}/bin/bash

    function show_list() {
        contents=("$@")
        for i in "''${contents[@]}";
        do
            echo "$i"
        done
    }

    function ensure_config_exists() {
        if [[ ! -f $${nasConfigPath} ]]; then
            ${dunst}/bin/dunstify -t 5000 -u critical "Missing config file, exiting"
            exit 1
        fi
    }

    function ensure_nas_online() {
        if [ -z "$$(${netcat}/bin/nc -z $NAS_HOSTNAME 22 2 -w 2 2>&1)" ]; then
            ${dunst}/bin/dunstify -t 7000 -u critical "Cannot access NAS, network error"
            exit 1
        fi
    }

    function ensure_volume_already_mounted() {
        if [[ ! -z $(cat /etc/mtab | ${gnugrep}/bin/grep catscan | ${coreutils}/bin/cut -d ' '  -f 1 | ${gnugrep}/bin/grep $VOLUME) ]]; then
            ${dunst}/bin/dunstify -t 5000 -u critical "Volume '$VOLUME' already mounted"
            exit 1
        fi
    }

    function mount_volume() {
        VOLUME=$1
        mkdir -p $NAS_MOUNT_PATH/$VOLUME
        ${afpfs-ng}/bin/mount_afp afp://$NAS_ADMIN_LOGIN:$NAS_ADMIN_PASSWORD@$NAS_HOSTNAME/$VOLUME \
            $NAS_MOUNT_PATH/$VOLUME
        if [[ $? -eq 0 ]]; then
            ${dunst}/bin/dunstify -t 3000 "Volume '$VOLUME' succesfully mounted"
        else
            ${dunst}/bin/dunstify -t 5000 -u critical "Error mounting volume '$VOLUME'"
        fi
    }

    NAS_HOSTNAME=$(${shyaml}/bin/shyaml -gy nas.hostname $CONFIGFILE)
    NAS_ADMIN_LOGIN=$(${shyaml}/bin/shyaml -gy nas.users.admin.login $CONFIGFILE)
    NAS_ADMIN_PASSWORD=$(${shyaml}/bin/shyaml -gy nas.users.admin.password $CONFIGFILE)
    NAS_MOUNT_PATH=$(${shyaml}/bin/shyaml -gy nas.mount.basedir $CONFIGFILE)

    nas_volumes=(
    $(${shyaml}/bin/shyaml -gy nas.volumes $${nasConfigPath})
    )

    main() {
        ensure_config_exists
        ensure_nas_online
        ensure_volume_already_mounted

        selected_volume=$( (show_list "''${nas_volumes[@]}") | ${rofi}/bin/rofi -dmenu -p "Mount: " )
        if [ -n "$selected_volume" ]; then
            mount_volume "$selected_volume"
        fi
    }

    main

    exit 0
''
