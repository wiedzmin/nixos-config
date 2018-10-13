{ config, pkgs, lib, ... }:

{
    imports = [
        ../private/hometraits.nix
    ];
    powerManagement = {
        enable = true;
        resumeCommands = (lib.concatMapStrings
                          (service: "${pkgs.systemd}/bin/systemctl try-restart ${service}\n")
                          config.sys.services_to_survive_suspend);
    };
    services.tlp.enable = true;
    services.acpid.enable = true;
}
