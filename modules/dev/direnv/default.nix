{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.direnv;
  user = config.attributes.mainUser.name;
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
        };
        xdg.configFile."direnv/direnv.toml".text = toToml {
          global = { disable_stdin = true; };
          whitelist = { prefix = cfg.whitelist; };
        };
      };
      dev.batchvcs.commands = { direnv = [ "[ -f .envrc ] && direnv allow || exit 0" ]; };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs:
        lib.optionals (cfg.emacs.granularity == "project") [ epkgs.direnv ]
        ++ lib.optionals (cfg.emacs.granularity == "file") [ epkgs.nix-buffer epkgs.envrc ];
      ide.emacs.core.config = readSubstituted [ ./subst.nix ] [ ./emacs/direnv.el ];
    })
  ];
}
