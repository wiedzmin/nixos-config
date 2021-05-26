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
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      console.useXkbConfig = true;

      home-manager.users."${user}" = {
        programs.readline = {
          enable = true;
          extraConfig = ''
            set echo-control-characters off
          '';
        };
        programs.zsh.sessionVariables = {
          YSU_IGNORED_ALIASES = [ "g" "ll" ];
          YSU_MODE = "ALL";
        };
        programs.fzf.enable = true;
        programs.command-not-found = {
          enable = true;
          dbPath = ./assets/programs.sqlite;
        };
        home.packages = with pkgs; [
          libnotify # FWIW
          perl # for plugins
          bashate # linter
          fancy-motd
        ];
      };
    })
    (mkIf (cfg.enable && cfg.dev.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ checkbashisms shellcheck ]; };
    })
    (mkIf (cfg.enable && cfg.dev.enable && cfg.emacs.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ nodePackages.bash-language-server ]; };
      ide.emacs.core.extraPackages = epkgs: [ epkgs.flycheck-checkbashisms ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/shell.el;
    })
  ];
}
