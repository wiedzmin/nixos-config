{...}:
{
  nix.nixPath = [
    "nixpkgs=/etc/nixos/pkgs/nixpkgs-channels"
    "nixpkgs-overlays=/etc/nixos/overlay"
    "nixos-config=/etc/nixos/configuration.nix"
    "home-manager=/etc/nixos/pkgs/home-manager"
  ];
}
