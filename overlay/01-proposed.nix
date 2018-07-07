self: super:
let nixpkgs = import ../nixpkgs-proposed {
      config = {
        allowUnfree = true;
      };
    };
    proposed = nixpkgs.pkgs;
in {
  inherit proposed;
}
