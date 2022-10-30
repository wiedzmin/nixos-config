{ config, lib, pkgs, ... }:
with config.ide.emacs;

let
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
rec {
  python3Binary = "${pkgs.python3}/bin/python3";
  websearchBinary = "${nurpkgs.toolbox}/bin/websearch";
  projectsRootMarkersEmacs = builtins.concatStringsSep " " (lib.forEach config.dev.navigation.projects.rootMarkers (marker: ''"${marker}"''));
  autorevertEnable = if edit.autorevert.enable then ":hook (dired-mode-hook . auto-revert-mode)" else "";
  # NOTE: Selectrum support has been recently deprecated in favor of Vertico
  consultDirEnable = if navigation.completion.backend == "selectrum" then ":disabled" else "";
  projectRootSexp = lib.optionalString (navigation.projects.backend == "project") "(project-root (project-current))" +
    lib.optionalString (navigation.projects.backend == "projectile") "(projectile-project-root)";
  projectBackendRequire = lib.optionalString (navigation.projects.backend == "project") "(require 'project)" +
    lib.optionalString (navigation.projects.backend == "projectile") "(require 'projectile)";
  consultDirProjectListFunction = lib.optionalString (navigation.projects.backend == "project") "consult-dir-project-dirs" +
    lib.optionalString (navigation.projects.backend == "projectile") "consult-dir-projectile-dirs";
  projectsSearchPaths = with config.navigation.bookmarks.workspaces;
    builtins.concatStringsSep " " (lib.forEach [ globalRoot globalRootStale ] (root: ''"${root}"''));
}
