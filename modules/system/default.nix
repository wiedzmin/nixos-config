{ ... }: {
  imports = [
    ./battery-notifier.nix
    ./paperworks.nix
    ./polkit-silent-auth.nix
    ./virt.nix
    ./xinput.nix
  ];
}
