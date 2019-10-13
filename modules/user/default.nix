{ ... }: {
  imports = [
    ./appearance.nix
    ./clean-trash.nix
    ./email.nix
    ./git.nix
    ./python.nix
    ./screenshots.nix
    ./xidlehook.nix
    ./xsuspender.nix # fix HM one and migrate to it, then try pushing upstream
  ];
}
