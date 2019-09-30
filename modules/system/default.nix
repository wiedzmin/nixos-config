{ ... }: {
  imports = [
    ./battery-notifier.nix
    ./media.nix
    ./network.nix
    ./paperworks.nix
    ./polkit-silent-auth.nix
    ./virt.nix
    ./xinput.nix
  ];
}
