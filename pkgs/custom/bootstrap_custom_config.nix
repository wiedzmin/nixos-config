{ bash, ... }:
''
    #!${bash}/bin/bash

    ROOT_PARTITION_LABEL=nixos-root
    BOOT_PARTITION_LABEL=nixos-boot
    MACHINE=''${1:-laptoptop}
    USERNAME=''${2:-alex3rd}
    PRIVATE_STORAGE_HOST=''${3:-localhost}
    PRIVATE_STORAGE_HOST_PORT=''${4:-8080}

    echo "Mounting root partition"
    mount /dev/disk/by-label/$ROOT_PARTITION_LABEL /mnt
    echo "creating /boot"
    mkdir -p /mnt/boot
    echo "Mounting boot partition"
    mount /dev/disk/by-label/$BOOT_PARTITION_LABEL /mnt/boot

    echo "Installing essential tools"
    nix-env -iA nixos.pkgs.gitAndTools.gitFull
    nix-env -iA nixos.wget

    echo "removing existing configuration tree, if any"
    rm -rf /mnt/etc/nixos

    git clone https://github.com/wiedzmin/nixos-config nixos

    # prepare config (fetch home-manager + nixpkgs)
    echo "preparing submodules"
    cd /mnt/etc/nixos && git submodule init && git submodule update
    echo "fetching private user data"
    # TODO: make more declarative/self-contained
    # TODO: harden in terms of security
    cd "/mnt/etc/nixos/users/$USERNAME" && wget "http://$PRIVATE_STORAGE_HOST:$PRIVATE_STORAGE_HOST_PORT/''${USERNAME}_private.tar.gz"
    if [ $? -ne 0 ]; then
        echo "Error fetching private data, check manually"
        exit 1
    fi

    echo "symlinking configuration root"
    cd /mnt/etc/nixos && ln -rsvf "machines/$MACHINE.nix" ./configuration.nix

    echo "actually installing"
    # home-manager and nixpkgs paths are either absent in NIX_PATH or point to wrong locations
    nixos-install --root /mnt -I home-manager=/mnt/etc/nixos/pkgs/home-manager-proposed -I nixpkgs=/mnt/etc/nixos/pkgs/nixpkgs-channels
''
