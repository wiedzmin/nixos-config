{ config, pkgs, lib, ... }:

{
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
