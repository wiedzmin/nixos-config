{
  description = "Staging packages sandbox environment";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    nur.url = github:wiedzmin/NUR;
    unstable.url = github:NixOS/nixpkgs/<ref>; # left intentinally incorrect,
                                               # because staging packages are mostly come from unstable HEAD
  };

  outputs = { self, flake-utils, unstable, nur }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import unstable {
          inherit system;
          overlays = [ nur.overlay ];
        };
      in
      { devShell = import ./shell.nix { inherit pkgs; }; });
}
