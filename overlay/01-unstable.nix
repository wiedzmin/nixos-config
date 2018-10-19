self: super:
let nixpkgs = import ../nixpkgs-channels {
      config = {
        allowUnfree = true;
      };
    };
in {
  unstable = nixpkgs.pkgs;
}
