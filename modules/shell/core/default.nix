{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.shell.core;
  user = config.attributes.mainUser.name;
  serviceAttrsNames = [ "global" "emacs" ];
  envVars = filterAttrs (name: _: !builtins.elem name serviceAttrsNames);
in {
  options = {
    shell.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable core shell setup";
      };
      variables = mkOption {
        type = types.listOf types.attrs;
        default = [ ];
        description = "Metadata-augmented environment variables registry";
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

      environment.variables = foldl (a: b: a // (envVars b)) { }
        (builtins.filter (e: builtins.hasAttr "global" e && e.global) cfg.variables);
      environment.sessionVariables = foldl (a: b: a // (envVars b)) { }
        (builtins.filter (e: builtins.hasAttr "global" e && e.global) cfg.variables);
      ide.emacs.core.environment = foldl (a: b: a // (envVars b)) { }
        (builtins.filter (e: builtins.hasAttr "emacs" e && e.emacs) cfg.variables);

      home-manager.users."${user}" = {
        programs.readline = {
          enable = true;
          extraConfig = ''
            set echo-control-characters off
          '';
        };
        home.sessionVariables = foldl (a: b: a // (envVars b)) { } cfg.variables;
        programs.zsh.sessionVariables = {
          YSU_IGNORED_ALIASES = [ "g" "ll" ];
          YSU_MODE = "ALL";
        } // (foldl (a: b: a // (envVars b)) { } cfg.variables);
        programs.bash.sessionVariables = foldl (a: b: a // (envVars b)) { } cfg.variables;
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
      ide.emacs.core.config = builtins.readFile ./emacs/shell.el;
    })
  ];
}
