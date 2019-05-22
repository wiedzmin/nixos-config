#!/usr/bin/env bash

MACHINES_CONFIG_PATH=./machines
MACHINE_CONFIGS=(
  $(fd --maxdepth 1 --type f '.nix' $MACHINES_CONFIG_PATH -x echo '{/.}')
)

list_machine_configs() {
    for i in "${MACHINE_CONFIGS[@]}"
    do
        echo "$i"
    done
}

main() {
    SELECTED_CONFIG=$( (list_machine_configs) | rofi -dmenu -p "config")
    if [ -n "$SELECTED_CONFIG" ]; then
        sudo nixos-rebuild -I nixos-config="$MACHINES_CONFIG_PATH/$SELECTED_CONFIG.nix" build-vm
    fi
}

main

exit 0
