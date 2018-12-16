{ config, pkgs, ...}:

{
    programs = {
        gnupg.agent = {
            enable = true;
            enableSSHSupport = true;
        };
    };

    environment.systemPackages = with pkgs; [
        (pass.withExtensions (ext: with ext; [ pass-audit pass-import pass-update ]))
        ejson
        gnupg
        gpa
        hashcat
        kbfs
        keybase
        keybase-gui
        lynis
        paperkey
        rofi-pass
        srm
        sslscan
        super
        twa
   ];
}
