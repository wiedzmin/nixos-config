{ bash, nix, systemd, ... }:
''
    #!${bash}/bin/bash

    cd /etc/nixos
    ${nix}/bin/nix build -f ./pkgs/forges/github.com/NixOS/nixpkgs-channels/nixos system $@
    result=$?
    if [[ $result == 1 ]] || [[ $result == 100 ]]
    then
        exit 1
    fi

    dir=$(pwd)
    export SHELL=/bin/sh
    # FIXME: sudo --> pkexec after figuring out the correct way to use the latter
    sudo ${nix}/bin/nix-env --profile /nix/var/nix/profiles/system --set $(readlink $dir/result)
    sudo $dir/result/bin/switch-to-configuration switch

    current=$(readlink -f /run/current-system/kernel)
    booted=$(readlink -f /run/booted-system/kernel)

    if [ "$current" != "$booted" ]; then
        read -p "Kernel changed, reboot? " -n 1 -r
        if [[ $REPLY =~ ^[Yy]$ ]]
        then
            echo "Rebooting in 5 sec..."
            sleep 5
            echo "kernel changed, reboot" | ${systemd}/bin/systemd-cat --identifier "post-upgrade-check";
            reboot
        fi
    else
        echo "same kernel, do not reboot" | ${systemd}/bin/systemd-cat --identifier "post-upgrade-check";
    fi
''
