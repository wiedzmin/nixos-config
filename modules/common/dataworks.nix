{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.dataworks;
  emacsCodeSearchSetup = ''
    (use-package codesearch
      :ensure t
      :custom
      (codesearch-global-csearchindex "${config.secrets.dev.workspaceRoot}/.csearchindex"))

    (use-package counsel-codesearch
      :ensure t
      :after codesearch
      :bind
      (:map mode-specific-map
            ("c" . counsel-codesearch)))

    (use-package projectile-codesearch
      :ensure t
      :after codesearch)
  '';
in {
  options = {
    dataworks = {
      codesearch.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Codesearch dev infra.";
      };
      structured.json.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable structured JSON manipulation tools.";
      };
      toolsng.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable successors of some traditional tools like find, sed, etc.";
      };
      forensics.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''Whether to enable various "forensics" tools.'';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.codesearch.enable {
      ide.emacs.config = ''${emacsCodeSearchSetup}'';
      # TODO: timer + service for reindexing + https://github.com/abingham/emacs-codesearch for emacs
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          codesearch
        ];
        programs = {
          zsh.sessionVariables = {
            CSEARCHINDEX = "${config.secrets.dev.workspaceRoot}/.csearchindex";
          };
          bash.sessionVariables = { # TODO: check if option indeed exists
            CSEARCHINDEX = "${config.secrets.dev.workspaceRoot}/.csearchindex";
          };
          emacs.extraPackages = epkgs: [ # TODO: correlate with navigation.enable somehow
            epkgs.codesearch
            epkgs.counsel-codesearch
            epkgs.projectile-codesearch
          ];
        };
      };
    })
    (mkIf cfg.structured.json.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          jl
          jid
          gron
          yj
        ];
        programs.jq = {
          enable = true;
          colors = {
            null = "1;30";
            false = "0;91";
            true = "0;92";
            numbers = "0;36";
            strings = "1;96";
            arrays = "1;94";
            objects = "1;33";
          };
        };
      };
    })
    (mkIf cfg.toolsng.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          fd
          lsd
          sd
        ];
      };
    })
    (mkIf cfg.forensics.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          bbe
          datamash
          dateutils
          diffoscope
          glogg
          icdiff
          pdfgrep
          q-text-as-data
          ripgrep-all
          textql
          up
          uq
          visidata # TODO: make overlay
          xsv
          xurls
        ] ++ lib.optionals (config.attributes.staging.enable) [
          lv
        ];
      };
    })
  ];
}
