{ config, inputs, lib, pkgs, ... }:

rec {
  emacsConsultProjectilePath = inputs.emacs-consult-projectile;
  emacsBookmarkViewPath = inputs.emacs-bookmark-view;
  python3Binary = "${pkgs.python3}/bin/python3";
  projectsRootMarkersEmacs = builtins.concatStringsSep " " (lib.forEach config.dev.navigation.projects.rootMarkers (marker: ''"${marker}"''));
}
