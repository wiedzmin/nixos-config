self: super:
let nixpkgs = import ../pkgs/nixpkgs-channels { config = { allowUnfree = true; }; };
in { unstable = nixpkgs.pkgs; }
