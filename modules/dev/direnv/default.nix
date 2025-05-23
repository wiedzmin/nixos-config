{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.direnv;
  user = config.attributes.mainUser.name;
  toml = pkgs.formats.toml { };
in
{
  options = {
    dev.direnv = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable direnv.";
      };
      whitelist = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "The collection of whitelisting prefixes for direnv to allow";
      };
      hideEnvDiff = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to environment variables diff during direnv status change";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra for Emacs.";
      };
      emacs.granularity = mkOption {
        type = types.enum [ "project" "file" ];
        default = "project";
        description = "The approach to switching direnv environments under Emacs";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      dev.git.core.gitignore = ''
        .direnv/
      '';
      home-manager.users."${user}" = {
        programs.direnv = {
          enable = true;
          nix-direnv.enable = true;
          enableZshIntegration = true;
        };
        xdg.configFile."direnv/direnv.toml".source = toml.generate "direnv.toml" {
          global = {
            disable_stdin = true;
            hide_env_diff = cfg.hideEnvDiff;
          };
          whitelist = { prefix = cfg.whitelist; };
        };
      };
      dev.vcs.batch.commands = { direnv = [ "[ -f .envrc ] && direnv allow || exit 0" ]; };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs:
        lib.optionals (cfg.emacs.granularity == "project") [ epkgs.direnv ]
        ++ lib.optionals (cfg.emacs.granularity == "file") [ epkgs.nix-buffer epkgs.envrc ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst/direnv.nix ] [ ./elisp/direnv.el ];
    })
  ];
}
