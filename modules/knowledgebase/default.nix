{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.knowledgebase;
  user = config.attributes.mainUser.name;
in
{
  options = {
    knowledgebase = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable knowledge base facilities.";
      };
      secondBrain.enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to enable "second brain" functionality, such as various
          information organizing tools, mindmaps implementations, etc.
        '';
      };
      man.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable man pages.";
      };
      info.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable TeXInfo pages.";
      };
      doc.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable documentation.";
      };
      dev.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable dev documentation.";
      };
      nixos.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable NixOS documentation.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs documentation setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      documentation = {
        enable = true;
        man.enable = cfg.man.enable;
        info.enable = cfg.info.enable;
        doc.enable = cfg.doc.enable;
        dev.enable = cfg.dev.enable;
        nixos = {
          inherit (cfg.nixos) enable;
          includeAllModules = false; # FIXME build error
        };
      };
      home-manager.users."${user}" = {
        programs = {
          info.enable = true;
          man.enable = true;
        };
      };
    })
    (mkIf (cfg.enable && cfg.secondBrain.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ devdocs-desktop heimer freeplane logseq ]; };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.helpful epkgs.which-key ];
      ide.emacs.core.config = builtins.readFile ./elisp/knowledgebase.el;
    })
  ];
}
