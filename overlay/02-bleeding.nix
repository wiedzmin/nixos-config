self: super:
let nixpkgs = import ../nixpkgs-master {
      config = {
        allowUnfree = true;
      };
    };
in {
  bleeding = nixpkgs.pkgs;
}
