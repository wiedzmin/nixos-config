{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.git.autopush;
in
{
  options = {
    dev.git.autopush = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Git VCS infrastructure.";
      };
      when = mkOption {
        type = types.str;
        default = "";
        description = "When to push updates (on calendar).";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.dev.vcs.batch.enable;
          message = "git: automatic updates pushing requires batch VCS setup to be enabled.";
        }
        {
          assertion = cfg.enable && cfg.when != "";
          message = "git: automatic updates pushing is enabled while not scheduled.";
        }
      ];
    })
  ];
}
