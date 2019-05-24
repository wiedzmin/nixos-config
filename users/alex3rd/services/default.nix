{config, pkgs, ...}:
{
    imports =
    [
        ./collect-garbage.nix
        ./fusuma.nix
        ./git-auto.nix
        ./imapfilter.nix
        ./nixpkgs-update-status.nix
        ./screenshots.nix
        ./sshuttle.nix
        ./vpn.nix
        ./xkeysnail.nix
        ./xserver.nix
        ./xsuspender.nix
    ];

    home-manager.users.alex3rd = {
        services.gpg-agent = {
            enable = true;
            defaultCacheTtl = 34560000;
            defaultCacheTtlSsh = 34560000;
            maxCacheTtl = 34560000;
            enableSshSupport = true;
            extraConfig = ''
                allow-emacs-pinentry
                allow-loopback-pinentry
            '';
        };
    };
}
