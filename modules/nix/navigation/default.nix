{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ext.nix.navigation;
  user = config.attributes.mainUser.name;
in
{
  options = {
    ext.nix.navigation = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable packaging infra.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix navigation infra for Emacs.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = { programs.zsh.shellAliases = { nlo = "${pkgs.nix-index}/bin/nix-locate --"; }; };
      systemd.user.services."nix-update-index" = {
        description = "Update nix packages metadata index";
        serviceConfig = {
          Type = "oneshot";
          CPUSchedulingPolicy = "idle";
          IOSchedulingClass = "idle";
          ExecStart = "${pkgs.nix-index}/bin/nix-index";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."nix-update-index" =
        renderTimer "Update nix packages metadata index" "" "" "*-*-* 6:00:00" false "";
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.customPackages = {
        "nix-tap" = { text = builtins.readFile ./elisp/custom/nix-tap.el; };
        "nix-entities" = { text = builtins.readFile ./elisp/custom/entities.el; };
      };

      ide.emacs.core.config = builtins.readFile ./elisp/navigation.el;
    })
  ];
}
