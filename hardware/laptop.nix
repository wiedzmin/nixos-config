{ config, pkgs, lib, ... }:

{
    powerManagement = {
        enable = true;
        # FIXME: ${pkgs.systemd}/bin/systemctl --user restart dunst.service
        resumeCommands = ''
            ${pkgs.systemd}/bin/systemctl restart imapfilter.service
            ${pkgs.systemd}/bin/systemctl try-restart sshuttle.service
        '';
        powertop.enable = true;
    };
    services.tlp.enable = true;
    services.acpid.enable = true;
}
