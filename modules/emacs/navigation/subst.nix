{ config, inputs, lib, pkgs, ... }:

rec {
  python3Binary = "${pkgs.python3}/bin/python3";
  projectsRootMarkersEmacs = builtins.concatStringsSep " " (lib.forEach config.dev.navigation.projects.rootMarkers (marker: ''"${marker}"''));
}
