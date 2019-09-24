{ ... }: {
  imports = [
    ./battery-notifier.nix
    ./paperwork.nix
    ./polkit-silent-auth.nix
    ./virt.nix
    ./xinput.nix
  ];
}
