{...}:
{
    imports =
    [
        ./clean-trash.nix
        ./fusuma.nix
        ./git-fetch-updates.nix
        ./git-push-updates.nix
        ./git-save-wip.nix
        ./battery-notifier.nix
        ./order-screenshots.nix
        ./sshuttle.nix
        ./vpn.nix
        ./xkeysnail.nix
        ./xsuspender.nix        # fix HM one and migrate to it, then try pushing upstream
    ];
}
