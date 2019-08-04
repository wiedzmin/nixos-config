{ bash, coreutils, dunst, gnugrep, shyaml, ... }:
let
    nasConfigPath = "$HOME/.config/synology/nas.yml";
in
''
    #!${bash}/bin/bash

    CONFIGFILE=${nasConfigPath}
    if [[ ! -f $CONFIGFILE ]]; then
        ${dunst}/bin/dunstify -t 5000 -u critical "Missing config file, exiting"
        exit 1
    fi
    NAS_HOSTNAME=$(${shyaml}/bin/shyaml -gy nas.hostname $CONFIGFILE)

    mounted_nas_volumes=$(cat /etc/mtab | ${gnugrep}/bin/grep $NAS_HOSTNAME | ${coreutils}/bin/cut -d ' '  -f 1)
    for i in "''${mounted_nas_volumes[@]}"
    do
        ${custom.unmount_nas_volume}/bin/unmount_nas_volume "$i"
    done
''
