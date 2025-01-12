{ pkgs, ... }:

let
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  projectsBinary = "${nurpkgs.toolbox}/bin/projects";
}
