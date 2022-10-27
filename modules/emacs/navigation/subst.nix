{ config, lib, pkgs, ... }:

let
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
rec {
  python3Binary = "${pkgs.python3}/bin/python3";
  websearchBinary = "${nurpkgs.toolbox}/bin/websearch";
  projectsRootMarkersEmacs = builtins.concatStringsSep " " (lib.forEach config.dev.navigation.projects.rootMarkers (marker: ''"${marker}"''));
  autorevertEnable = if config.ide.emacs.edit.autorevert.enable then ":hook (dired-mode-hook . auto-revert-mode)" else "";
}
