{ config, pkgs, ...}:

{
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
        pinentry
        rofi-pass
        srm
        sslscan
        super
        twa
   ];
}
