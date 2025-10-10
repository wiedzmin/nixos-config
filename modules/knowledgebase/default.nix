{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# nsp>freeplane npkg#freeplane

let
  cfg = config.knowledgebase;
  user = config.attributes.mainUser.name;
  nixpkgs-last-unbroken = import inputs.nixpkgs-last-unbroken {
    config = config.nixpkgs.config // {
      allowUnfree = true;
      permittedInsecurePackages = config.ext.nix.core.permittedInsecurePackages;
    };
    localSystem = { system = "x86_64-linux"; };
  };
in
{
  options = {
    knowledgebase = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable knowledge base facilities.";
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
          includeAllModules = false; # NOTE build error
        };
      };
      home-manager.users."${user}" = {
        programs = {
          info.enable = true;
          man.enable = true;
        };
        home.packages = with pkgs; [
          glibcInfo
          man-pages
        ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.helpful ];
      ide.emacs.core.config = builtins.readFile ./elisp/knowledgebase.el;
    })
  ];
}
