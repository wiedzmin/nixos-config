{
  description = "Media conversion environment";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    nur.url = github:wiedzmin/NUR;
    unstable.url = github:NixOS/nixpkgs/{{ .inputsUnstableRev }};
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
