{...}:
{
  nix.nixPath = [
    "nixpkgs=/etc/nixos/nixpkgs-proposed"
    "nixpkgs-overlays=/etc/nixos/overlay"
    "nixos-config=/etc/nixos/configuration.nix"
  ];
}
