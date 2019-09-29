{ ... }: {
  imports = [
    ./battery-notifier.nix
    ./media.nix
    ./paperworks.nix
    ./polkit-silent-auth.nix
    ./virt.nix
    ./xinput.nix
  ];
}
