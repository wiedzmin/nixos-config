{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.shell.core;
  user = config.attributes.mainUser.name;
in {
  options = {
    shell.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable core shell setup";
      };
      dev.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell-related development infra";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell-related Emacs infra";
      };
      emacs.spellcheck.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell-related spellchecking for Emacs";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        programs.readline = {
          enable = true;
          extraConfig = ''
            set echo-control-characters off
          '';
        };
        programs.command-not-found = {
          enable = true;
          dbPath = ./assets/programs.sqlite;
        };
        home.packages = with pkgs; [
          libnotify # FWIW
          perl # for plugins
        ];
      };
    })
    (mkIf (cfg.enable && cfg.dev.enable) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ checkbashisms nodePackages.bash-language-server shellcheck ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable && emacs.spellcheck.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.flycheck-checkbashisms ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/shell.el;
    })
  ];
}
