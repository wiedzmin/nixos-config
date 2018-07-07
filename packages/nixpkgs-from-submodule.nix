{...}:
{
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nixpkgs"
    "nixpkgs-overlays=/etc/nixos/overlay"
    "nixos-config=/etc/nixos/configuration.nix"
  ];
}
