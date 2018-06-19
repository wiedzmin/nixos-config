{ config, lib, pkgs, ... }:

{
    # TODO: access (uncomment) by labels instead of UUIDs
    fileSystems."/" = {
        device = "/dev/disk/by-uuid/a68e6347-8bbe-462a-a02b-5dcf2df3d9d8";
        # device = "/dev/disk/by-label/nixos-root";
        fsType = "ext4";
    };

    fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/efd508ea-0f7a-4c92-982a-c628b7a89a69";
        # device = "/dev/disk/by-label/nixos-boot";
        fsType = "ext2";
    };

    swapDevices = [ ];

}
