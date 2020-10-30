{
  description = "Development environment";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    unstable.url = github:NixOS/nixpkgs/8e56330ad9a6e254ed0c4a0113e93cc880f87df5;
    nur.url = "/home/alex3rd/workspace/repos/github.com/wiedzmin/NUR";
  };

  outputs = { self, flake-utils, unstable, nur }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import unstable {
          inherit system;
          overlays = [ nur.overlay ];
        });
      in { devShell = import ./shell.nix { inherit pkgs; }; });
}
