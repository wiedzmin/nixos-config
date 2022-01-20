{ pkgs, ... }:

let
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
rec {
  projectsBinary = "${nurpkgs.toolbox}/bin/projects";
}
