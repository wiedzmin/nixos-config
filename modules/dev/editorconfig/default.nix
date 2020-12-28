{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.editorconfig;
  user = config.attributes.mainUser.name;
in {
  options = {
    dev.editorconfig = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable custom development infrastructure.";
      };
      rules = mkOption {
        type = types.attrs;
        default = { };
        description = "Editorconfig global rules";
      };
      basicRules = mkOption { # TODO: consider extracting options
        type = types.attrs;
        default = {
          "charset" = "utf-8";
          "end_of_line" = "lf";
          "indent_style" = "space";
          "insert_final_newline" = true;
          "trim_trailing_whitespace" = true;
        };
        description = "Editorconfig global rules";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra for Emacs.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ editorconfig-checker ];
        home.file = {
          "${config.custom.navigation.workspaceRootGlobal}/.editorconfig".text = ''
            root = true

          '' + generators.toINI { } ({
            "*" = cfg.basicRules;
          } // cfg.rules);
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.editorconfig ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/ec.el;
    })
  ];
}
