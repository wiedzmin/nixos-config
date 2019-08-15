{...}:
{
    imports =
    [
        ./battery-notifier.nix
        ./clean-trash.nix
        ./docker-dns.nix
        ./fusuma.nix
        ./git-fetch-updates.nix
        ./git-push-updates.nix
        ./git-save-wip.nix
        ./order-screenshots.nix
        ./sshuttle.nix
        ./xkeysnail.nix
        ./xidlehook.nix
        ./xpointerbarrier.nix
        ./xsuspender.nix        # fix HM one and migrate to it, then try pushing upstream
    ];
}
