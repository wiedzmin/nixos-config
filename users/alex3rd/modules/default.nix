{...}:
{
    imports =
    [
        ./clean-trash.nix
        ./fusuma.nix
        ./git-fetch-updates.nix
        ./git-push-updates.nix
        ./git-save-wip.nix
        ./keep-vpn.nix
        ./lowbatt.nix
        ./order-screenshots.nix
        ./sshuttle.nix
        ./xkeysnail.nix
        ./xsuspender.nix        # fix HM one and migrate to it, then try pushing upstream
    ];
}
