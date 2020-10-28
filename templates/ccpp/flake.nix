{
  description = "Development environment";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/8e56330ad9a6e254ed0c4a0113e93cc880f87df5;

  outputs = { self, nixpkgs }: {
    devShell."x86_64-linux" = import ./shell.nix { pkgs = nixpkgs.legacyPackages.x86_64-linux; };
  };
}
