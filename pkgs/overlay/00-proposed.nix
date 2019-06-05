self: super:
let nixpkgs = import ../nixpkgs-proposed {
      config = {
        allowUnfree = true;
      };
    };
in {
  proposed = nixpkgs.pkgs;
}
