{ config, inputs, lib, pkgs, ... }:
with config.ide.emacs;

let
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  python3Binary = "${pkgs.python3}/bin/python3";
  websearchBinary = "${nurpkgs.toolbox}/bin/websearch";
  projectsRootMarkersEmacs = builtins.concatStringsSep " " (lib.forEach config.dev.navigation.projects.rootMarkers (marker: ''"${marker}"''));
  autorevertEnable = if edit.autorevert.enable then ":hook (dired-mode-hook . auto-revert-mode)" else "";
  projectRootSexp = lib.optionalString (navigation.projects.backend == "project") "(project-root (project-current))" +
    lib.optionalString (navigation.projects.backend == "projectile") "(projectile-project-root)";
  projectBackendRequire = lib.optionalString (navigation.projects.backend == "project") "(require 'project)" +
    lib.optionalString (navigation.projects.backend == "projectile") "(require 'projectile)";
  consultDirProjectListFunction = lib.optionalString (navigation.projects.backend == "project") "consult-dir-project-dirs" +
    lib.optionalString (navigation.projects.backend == "projectile") "consult-dir-projectile-dirs";
  projectsSearchPaths = with config.navigation.bookmarks.workspaces;
    builtins.concatStringsSep " " (lib.forEach [ globalRoot ] (root: ''"${root}"''));
  selectionCandidatesCount = builtins.toString navigation.selection.candidatesCount;
  emacsEpithetPath = inputs.emacs-epithet;
  emacsProjectHeaderlinePath = inputs.emacs-project-headerline;
  recenterWindowDisabled = if config.ide.emacs.navigation.customWindowRecentering.enable then "" else ":disabled";
  recenterWindowEyeLevel = lib.strings.floatToString config.ide.emacs.navigation.customWindowRecentering.eyeLevel;
  currentLineHighlightFaceVerticoPatch = if config.appearance.emacs.currentLineHighlightFace != "" then '':custom-face (vertico-current ((t (:background "${config.appearance.emacs.currentLineHighlightFace}"))))'' else "";
}
