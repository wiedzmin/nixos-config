{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.git.autofetch;
in
{
  options = {
    dev.git.autofetch = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable batch updates fetching.";
      };
      defaultUpstreamRemote = mkOption {
        type = types.str;
        default = "upstream";
        description = "Name of upstream repo remote.";
      };
      defaultOriginRemote = mkOption {
        type = types.str;
        default = "origin";
        description = "Name of origin repo remote.";
      };
      mainBranchName = mkOption {
        type = types.str;
        default = "master";
        description = "Subj.";
      };
      when = mkOption {
        type = types.str;
        default = "";
        description = "When to fetch updates (on calendar).";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.dev.vcs.batch.enable;
          message = "git: automatic updates fetching requires batch VCS setup to be enabled.";
        }
        {
          assertion = cfg.enable && cfg.when != "";
          message = "git: automatic updates fetching is enabled while not scheduled.";
        }
      ];
    })
  ];
}
