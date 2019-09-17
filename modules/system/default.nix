{ ... }: {
  imports = [
    ./battery-notifier.nix
    ./clean-trash.nix
    ./docker-dns.nix
    ./fusuma.nix
    ./git-fetch-updates.nix
    ./git-push-updates.nix
    ./git-save-wip.nix
    ./keynav.nix
    ./order-screenshots.nix
    ./xidlehook.nix
    ./xkeysnail.nix
    ./xpointerbarrier.nix
    ./xsuspender.nix # fix HM one and migrate to it, then try pushing upstream
  ];
}
