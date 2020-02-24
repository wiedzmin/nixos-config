{ ... }: {
  imports = [
    ./dev
    ./identity
    ./job/14f7646bef
    ./job/b354e944b3
    ./networking

    ./emacs.nix
    ./email.nix
    ./nas.nix
    ./navigation.nix
  ];
}
