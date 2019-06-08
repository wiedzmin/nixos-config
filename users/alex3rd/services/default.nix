{config, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
{
    imports =
    [
        ./fusuma.nix
        ./git-auto.nix
        ./imapfilter.nix
        ./nixpkgs-update-status.nix
        ./screenshots.nix
        ./sshuttle.nix
        ./vpn.nix
        ./xkeysnail.nix
        ./xsuspender.nix
    ];

    home-manager.users."${userName}" = {
        services.gpg-agent = {
            enable = true;
            defaultCacheTtl = 34560000;
            defaultCacheTtlSsh = 34560000;
            maxCacheTtl = 34560000;
            enableSshSupport = true;
            enableExtraSocket = true;
            extraConfig = ''
                allow-emacs-pinentry
                allow-loopback-pinentry
            '';
        };
    };
}
