{ config, inputs, pkgs, ... }:
with config.ide.emacs;

let
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  autorevertEnable = if edit.autorevert.enable then ":hook (dired-mode-hook . auto-revert-mode)" else "";
  websearchBinary = "${nurpkgs.toolbox}/bin/websearch";
  orgRoamRoot = config.pim.orgmode.org-roam.rootDir;
  emacsEpithetPath = inputs.emacs-epithet;
  emacsProjectHeaderlinePath = inputs.emacs-project-headerline;
}
