{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.dataworks;
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
    custom.dataworks = {
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
      tex.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various TeX tools.";
      };
      forensics.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''Whether to enable various "forensics" tools.'';
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''Whether to enable respective Emacs setup.'';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.codesearch.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          codesearch
        ];
        programs = {
          zsh.sessionVariables = {
            CSEARCHINDEX = "${config.secrets.dev.workspaceRoot}/.csearchindex";
          };
          bash.sessionVariables = {
            CSEARCHINDEX = "${config.secrets.dev.workspaceRoot}/.csearchindex";
          };
        };
      };
      systemd.user.services."codesearch-reindex" = {
        description = "Codesearch index updating";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        serviceConfig = {
          Type = "oneshot";
          Environment = [
            "CSEARCHINDEX=${config.secrets.dev.workspaceRoot}/.csearchindex"
          ];
          ExecStart = "${pkgs.codesearch}/bin/cindex ${config.secrets.dev.workspaceRoot}";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
      systemd.user.timers."codesearch-reindex" = {
        description = "Codesearch index updating";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "5min";
          OnUnitActiveSec = "1h";
        };
      };
    })
    (mkIf (cfg.codesearch.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [
          epkgs.codesearch
          epkgs.counsel-codesearch
          epkgs.projectile-codesearch
        ];
      };
      ide.emacs.config = ''${emacsCodeSearchSetup}'';
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
          sd
        ] ++ lib.optionals config.attributes.staging.enable [
          fselect
        ];
        programs = {
          lsd = {
            enable = true;
            enableAliases = true;
          };
          bat = {
            enable = true;
            config = {
              theme = "TwoDark";
              pager = "less -FR";
            };
          };
        };
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
          ruplacer
          step-cli
          squashfs-tools-ng
          hexdino
          txr # TODO: get started, read docs
        ];
      };
    })
    (mkIf cfg.tex.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; lib.optionals (config.attributes.staging.enable) [
          pplatex
          texlab
        ];
      };
    })
  ];
}
