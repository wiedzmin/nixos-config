{ bash, coreutils, perl, unzip, ... }:
''
    #!${bash}/bin/bash

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

    ${coreutils}/bin/rm -rf "$machine_name"
    ${unzip}/bin/unzip "$machine_archive" -d "$machine_name" > /dev/null
    ${perl}/bin/perl -pi -e "s|__MACHINE__STORAGE_PATH__|$MACHINE_STORAGE_PATH|g" $machine_name/config.json
    ${coreutils}/bin/mv "$machine_name" "$MACHINE_STORAGE_PATH/machines"

    echo "Imported $machine_name to docker-machine ($machine_path)"
''
