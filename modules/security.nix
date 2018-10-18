{ config, pkgs, ...}:

{
    security.sudo.wheelNeedsPassword = false;

    security.polkit.extraConfig = ''
        /* Allow users in wheel group to manage systemd units without authentication */
        polkit.addRule(function(action, subject) {
            if (action.id == "org.freedesktop.systemd1.manage-units" &&
                subject.isInGroup("wheel")) {
                return polkit.Result.YES;
            }
        });
    '';

    environment.systemPackages = with pkgs; [
        (pass.withExtensions (ext: with ext; [ pass-audit pass-import pass-update ]))
        ejson
        gnupg
        paperkey
        rofi-pass
        srm
   ];
}
