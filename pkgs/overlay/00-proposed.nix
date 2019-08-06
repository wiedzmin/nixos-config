self: super:
let nixpkgs = import ../forges/github.com/wiedzmin/nixpkgs {
      config = {
        allowUnfree = true;
      };
    };
in {
  proposed = nixpkgs.pkgs;
}
