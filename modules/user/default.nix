{ ... }: {
  imports = [
    ./appearance.nix
    ./clean-trash.nix
    ./git-fetch-updates.nix
    ./git-push-updates.nix
    ./git-save-wip.nix
    ./screenshots.nix
    ./xidlehook.nix
    ./xsuspender.nix # fix HM one and migrate to it, then try pushing upstream
  ];
}
