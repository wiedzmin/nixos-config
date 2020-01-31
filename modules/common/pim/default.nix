{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.pim;
in {
  options = {
    custom.pim = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable personal informationmanagement infra/tools.";
      };
      org.warningsFile = mkOption { # TODO: consider commonalizing options names
        type = types.str;
        default = "$HOME/warnings.org";
        description = "Org-mode file to place accidental deletes diff.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs pim-related setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        tt_capture = writePythonScriptWithPythonPackages "tt_capture" [
          pkgs.python3Packages.cbor2
          pkgs.python3Packages.pytz
          pkgs.python3Packages.xlib
        ] (builtins.readFile
            (pkgs.substituteAll
              ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./tt_capture.py; })));
      };
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          remind # + rem2ics (make overlay)
          wyrd

          tt_capture
        ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          plantuml
        ];
        programs.emacs.extraPackages = epkgs: [
          epkgs.counsel-org-clock
          epkgs.ob-async
          epkgs.ob-blockdiag
          epkgs.ob-restclient
          epkgs.org-bullets
          epkgs.org-capture-pop-frame
          epkgs.org-clock-today
          epkgs.org-plus-contrib
          epkgs.org-pomodoro
          epkgs.org-randomnote
          epkgs.org-recent-headings
          epkgs.org-rich-yank
          epkgs.orgit
          epkgs.orglink
          epkgs.plantuml-mode
          epkgs.russian-holidays
        ];
      };
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./pim.el; }));
    })
  ];
}
