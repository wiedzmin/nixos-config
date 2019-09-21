{ ... }: {
  imports = [
    ./battery-notifier.nix
    ./clean-trash.nix
    ./docker-dns.nix
    ./git-fetch-updates.nix
    ./git-push-updates.nix
    ./git-save-wip.nix
    ./paperwork.nix
    ./polkit-silent-auth.nix
    ./screenshots.nix
    ./xidlehook.nix
    ./xinput.nix
    ./xsuspender.nix # fix HM one and migrate to it, then try pushing upstream
  ];
}
