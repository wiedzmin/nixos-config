{ config, pkgs, lib, ... }:

{
    imports = [
        ../private/hometraits.nix
    ];
    powerManagement = {
        enable = true;
        resumeCommands = (lib.concatMapStrings
                          (service: "${pkgs.systemd}/bin/systemctl try-restart ${service}\n")
                          config.sys.services_sleepless);
        powertop.enable = true;
    };
    services.tlp.enable = true;
    services.acpid.enable = true;
}
